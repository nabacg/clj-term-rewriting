(ns clj-term-rewriting.core
  (:require [clojure.core.match :refer [match]]))


(defrecord Expr [symbol term])

(defrecord Term [symbol exprs])


(defmacro rule [pattern expr]
  (fn [t]
    (match t
           pattern expr
           :else false)))

(def pass (rule x x))

(def fail (rule _ false))


(defn pipe [s p]
  (fn [t]
    (if-let [u (s t)]
      (p t)
      false)))

(defn alt [s & more]
  (fn [t]
    (let [u (s t)]
      (cond
        u u
        (empty? more) false
        :else ((apply alt more) t)))))

(def term? coll?) ;todo maybe seq?

(defn all [s]
  (fn [t]
    (if (term? t)
      (let [children (map s (rest t))]
        (if (some (partial = false) children)
          false
          (cons (first t) children))))))

(defn try-rule [s]
  (alt s pass))


(defn repeat [s]
  (fn [t]
    ((try-rule (pipe s (repeat s))) t)))

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
  (alt (rule `(not true) 'false)
       (rule `(not false) 'true)

       (rule `(and true ,x) x)
       (rule `(and ,x true) x)
       (rule `(and false ,x) 'false)
       (rule `(and ,x false) 'false)

       (rule `(or true ,x) 'true)
       (rule `(or ,x true) 'true)
       (rule `(or false ,x) x)
       (rule `(or ,x false) x)
       ))

(def evaluate (bottom-up (repeat evaluation)))


;(evaluate '(not true))

(evaluate '(and true unknown))
;(evaluate '(and (not false) (or true whatever)))
