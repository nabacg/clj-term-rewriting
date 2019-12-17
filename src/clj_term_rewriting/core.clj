(ns clj-term-rewriting.core
  (:require [clojure.core.match :refer [match matchv]]))

(defmacro rule [pattern expr]
  (let [pattern (if (coll? pattern) (vec pattern) [pattern])
        t (gensym)]
    `(fn [~t]
      (match [~t]
             [(~pattern :seq)] ~expr
             :else false))))

; (macroexpand-1 '(rule [:a 42] :nice))

;((rule [:a 42] :nice) 23)
(def pass (rule x x))

(def fail (rule _ nil))

;((rule [:a 42] :nice) [:a 42])

(defn pipe [s p]
  (fn [t]
    (if-let [u (s t)]
      (p u)
      nil)))

(defn alt [s & more]
  (fn [t]
    (let [u (s t)]
      (cond
        u u
        (empty? more) nil
        :else ((apply alt more) t)))))

(def term? seq?) ;todo maybe seq?

(defn all [s]
  (fn [t]
    (if (term? t)
      (let [children (map s (rest t))]
        (when (every? (comp not nil?) children)
          (cons (first t) children)
          ))
      t)))

(defn try-rule [s]
  (alt s pass))


(defn repeat-rule [s]
  (fn [t]
    ((try-rule (pipe s (repeat-rule s))) t)))

(defn top-down [s]
  (fn [t]
    ((pipe s (all (top-down s))) t)))

(defn bottom-up [s]
  (fn [t]
    ((pipe (all (bottom-up s)) s) t)))

(defn innermost [s]
  (fn [t]
    ((bottom-up (try-rule (pipe s (innermost s)))) t)))


(def evaluation
  (alt (rule (not true) false)
       (rule (not false) true)

       (rule (and true x) x)
       (rule (and x true) x)
       (rule (and false x) false)
       (rule (and x false) false)

       (rule (or true x) true)
       (rule (or x true) true)
       (rule (or false x) x)
       (rule (or x false) x)))

(def evaluate (bottom-up (repeat-rule evaluation)))

;; (evaluate '(not true))

;;(evaluate '(and true unknown))
;;(evaluate '(and (not false) (or true whatever)))

;; this works
;; ((pipe (all evaluation) evaluation) '(and (not false) (or true whatever)))

;; but pipe and all are not sufficient for one more level of nesting
;; ((pipe (all evaluation) evaluation) '(and (not false) (or true (and true whatever))))
;; ((all evaluation) '(and (not false) (or true (and true whatever))))
;; ((all evaluation) 'whatever)


;;https://github.com/brandonbloom/retree/blob/master/src/retree/core.clj
