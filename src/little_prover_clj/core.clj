(ns little-prover-clj.core
  (:require [com.rpl.specter :as sp]
            [com.rpl.specter.zipper :as spz]
            [clojure.walk :as w]))

;;; some slightly scheme-y core definitions
(defn prim? [x]
  (not (coll? x)))


;;; a simple unifier

;; 'proof' variables are unknowns that we can unify against real variables
(defn pvar [n]
  (gensym (str "pvar-" (name n))))

(defn pvar? [s]
  (and (symbol? s)
       (clojure.string/starts-with? (name s) "pvar-")))

(defn walk [smap x]
  (if (pvar? x)
    (if-let [bound-val (get smap x)]
      (recur smap (get smap x))
      x)
    x))

(defn unify [smap f1 f2]
  (let [f1 (walk smap f1)
        f2 (walk smap f2)]
    (cond
      (and (sequential? f1) (seq f1)) (-> smap
                                          (unify (first f1) (first f2))
                                          (unify (rest f1) (rest f2)))
      (= f1 f2) smap
      (pvar? f1) (assoc smap f1 f2)
      (pvar? f2) (assoc smap f2 f1)
      :default nil)))

;;; theorems are curried fns of a specter path and a form; the they transform
;;; the form at the given path, throwing if unable.

(defn check-focused-val [focused-val p q]
  (if-let [smap (unify {} focused-val p)]
    (walk smap q)
    (if-let [smap (unify {} focused-val q)]
      (walk smap p)
      (throw (ex-info "Can't apply theorem"
                      {:focused-val focused-val
                       :expr (list '= p q)})))))

(defn theorem [args body]
  (if-not (and (= 3 (count body))
               (= '= (first body)))
    (throw (ex-info "Body must be of the form (= x y)" {:body body}))
    ;; replace theorem args with pvars
    (let [arg-to-pvar-map (->> args
                               (map (juxt identity pvar))
                               (into {}))
          body (w/prewalk-replace arg-to-pvar-map body)
          [_ p q] body]
      (fn [focus-path]
        (fn [form]
          (sp/transform focus-path
                        #(check-focused-val % p q)
                        form))))))

;; theorem sugar
(defmacro defthm [name arg-vec body]
  `(def ~name (theorem (quote ~arg-vec) (quote ~body))))

;; A proof is a seq of theorems; apply one after another it gets down to a
;; boolean.
(defn check-proof [form proof]
  (cond
    (true? form) true
    (false? form) false
    (seq proof) (recur ((first proof) form)
                       (rest proof))
    :default {:result :inconclusive
              :last-form form}))

;;; axioms

;; special theorem to represent direct evaluation
(defn eval-form [focus-path]
  (fn [form]
    (sp/transform focus-path eval form)))

(defthm prim-cons [x y]
  (= (prim? (cons x y))
     false))

(defthm first-cons [x y]
  (= (first (cons x y))
     x))

(defthm rest-cons [x y]
  (= (rest (cons x y))
     y))
