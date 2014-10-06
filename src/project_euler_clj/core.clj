(ns project-euler-clj.core
  (:require [clojure.math.numeric-tower :as math]))

;; Problem 1
(defn problem-1 []
  (reduce + (filter #(or (= (mod % 3) 0)
                         (= (mod % 5) 0))
                    (range 1 1000))))

;; Problem 2
(defn fib
  ([] (fib 1 1))
  ([n1 n2] (cons n1 (lazy-seq (fib n2 (+ n2 n1))))))

(defn problem-2 []
  (reduce + (filter even? (take-while (partial > 4000000) (fib)))))

;; Problem 3
(defn prime? [num]
  (loop [test 2]
    (cond
     (= num 1) false
     (= num 2) true
     (= (mod num test) 0) false
     (> test (math/sqrt num)) true
     :else (recur (inc test)))))

(def primes (filter prime? (iterate inc 2)))

(defn prime-factorization [num]
  (let [root (math/sqrt num)]
    (filter #(= (mod num %) 0) (take-while (partial > root) primes))))

(defn largest-prime-factor [num]
  (loop [lpf nil
         primes primes]
    (when (> (first primes) (math/sqrt num)) lpf
          (recur (if (= (mod num (first primes)) 0) (first primes) lpf)
                 (rest primes)))))

;; Problem 4
(defn palindrome? [s]
  (= s (apply str (reverse s))))

(def mem-palindrome? (memoize palindrome?))

(def memoized-product (memoize *))

(defn mem-* [x y]
  (let [args (sort > [x y])]
    (apply memoized-product args)))

(defn problem-4 []
  (take 1 (sort >  (for [x (range 999 99 -1) y (range 999 99 -1)
                         :when (palindrome? (str (* x y)))]
                     (* x y)))))

;; Problem 5
(defn evenly-divisible? [num div]
  (loop [results []
         divisors div]
    (if (empty? divisors) (reduce #(and %1 %2) results)
        (recur (conj results (= (mod num (first divisors)) 0))
               (rest divisors)))))
(defn problem-5 []
  (loop [num 1]
    (if (and (= (mod num 17) 0) (evenly-divisible? num (range 1 21)))
      num
      (recur (inc num)))))
