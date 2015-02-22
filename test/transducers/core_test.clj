(ns transducers.core-test
  (:require [clojure.test :refer :all]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer :all]
            [transducers.core :refer :all]))

(def n-test 100)

(defspec mmap-test
         n-test
         (prop/for-all [coll (gen/not-empty (gen/vector gen/int))
                        f (gen/elements [inc dec #(* % %)])]
                       (= (map f coll)
                          (sequence (map f) coll)
                          (sequence (mmap f) coll))   ))

(defspec mfilter-test
         n-test
         (prop/for-all [coll (gen/not-empty (gen/vector gen/int))
                        p (gen/elements [even? odd? (partial > 5) (partial <= 8)])]
                       (= (filter p coll)
                          (sequence (filter p) coll)
                          (sequence (mfilter p) coll))))

(defspec mremove-test
         n-test
         (prop/for-all [coll (gen/not-empty (gen/vector gen/int))
                        p (gen/elements [even? odd? (partial > 5) (partial <= 8)])]
                       (= (remove p coll)
                          (sequence (remove p) coll)
                          (sequence (mremove p) coll))))

(defspec mtake-test
         n-test
         (prop/for-all [coll (gen/not-empty (gen/vector gen/int))
                        n gen/pos-int]
                       (= (take n coll)
                          (sequence (take n) coll)
                          (sequence (mtake n) coll))))

(defspec mdrop-test
         n-test
         (prop/for-all [coll (gen/not-empty (gen/vector gen/int))
                        n gen/pos-int]
                       (= (drop n coll)
                          (sequence (drop n) coll)
                          (sequence (mdrop n) coll))))

(defspec mtake-while-test
         n-test
         (prop/for-all [coll (gen/not-empty (gen/vector gen/int))
                        p (gen/elements [(partial > 5) (partial <= 10) odd? even?])]
                       (= (take-while p coll)
                          (sequence (take-while p) coll)
                          (sequence (mtake-while p) coll))))

(defspec mdrop-while-test
         n-test
         (prop/for-all [coll (gen/not-empty (gen/vector gen/int))
                        p (gen/elements [(partial > 5) (partial <= 10) odd? even?])]
                       (= (drop-while p coll)
                          (sequence (drop-while p) coll)
                          (sequence (mdrop-while p) coll))))

(defspec mmapcat-test
         n-test
         (prop/for-all [coll (gen/not-empty (gen/vector gen/int))
                        f (gen/elements [(partial repeat 10) (fn [x] [x x * x])])]
                       (= (flatten (map f coll))
                          (sequence (mapcat f) coll)
                          (sequence (mmapcat f) coll))))

(defspec muniq-test
         n-test
         (prop/for-all [coll (gen/not-empty (gen/vector gen/int))]
                       (= (distinct coll)
                          (sequence (muniq) coll))))

(defspec mrandom-sample-test
         n-test
         (prop/for-all [coll (gen/not-empty (gen/vector gen/int))
                        p (gen/choose 0 100)]
                       (<= (count (sequence (mrandom-sample (/ p 100)) coll))
                           (count coll))))
