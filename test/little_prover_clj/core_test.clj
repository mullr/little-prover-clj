(ns little-prover-clj.core-test
  (:require [clojure.test :refer :all]
            [little-prover-clj.core :refer :all]
            [com.rpl.specter :as sp]))

(deftest unify-test
  (are [f1 f2 smap] (= smap (unify {} f1 f2))
    '(car pvar-a pvar-b) '(car x y) '{pvar-a x, pvar-b y})
  )

(deftest match-form-test
  (is (= false (apply-replacement-rule '(prim? (cons a b))
                           '(prim? (cons pvar-x pvar-y)) false)))

  (is (= 'a (apply-replacement-rule '(first (cons a b))
                        '(first (cons pvar-x pvar-y)) 'pvar-x)))
  )

(deftest test-axioms
  (are [form proof] (check-proof form proof)

    '(first '(true false))
    [(eval-form [])]

    '(= false (prim? (cons a b)))
    [(prim-cons [sp/LAST])
     (eval-form [])]

    '(first (cons true b))
    [(first-cons [])]

    '(rest (cons a true))
    [(rest-cons [])]
    ))
