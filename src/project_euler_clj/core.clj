(ns project-euler-clj.core
  (:require [clojure.math.numeric-tower :as math]
            [clojure.string :as string]))

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
     ;; (= num 2) true
     (> (* test test) num) true
     (= (mod num test) 0) false
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

;; Problem 6
(defn sum-of-squares [nums]
  (reduce (fn [n1 n2]
            (+ n1 (* n2 n2))) nums))

(defn square-of-sums [nums]
  (let [sum (reduce + nums)]
    (* sum sum)))

(defn problem-6 []
  (let [nums (range 1 101)]
    (- (square-of-sums nums) (sum-of-squares nums))))

;; Problem 7
(defn problem-7 []
  (take 1 (drop 10000 primes)))

;; Problem 8
(def thousand-digits
  (map #(- (int %) 48) (string/replace
                        "73167176531330624919225119674426574742355349194934
                        96983520312774506326239578318016984801869478851843
                        85861560789112949495459501737958331952853208805511
                        12540698747158523863050715693290963295227443043557
                        66896648950445244523161731856403098711121722383113
                        62229893423380308135336276614282806444486645238749
                        30358907296290491560440772390713810515859307960866
                        70172427121883998797908792274921901699720888093776
                        65727333001053367881220235421809751254540594752243
                        52584907711670556013604839586446706324415722155397
                        53697817977846174064955149290862569321978468622482
                        83972241375657056057490261407972968652414535100474
                        82166370484403199890008895243450658541227588666881
                        16427171479924442928230863465674813919123162824586
                        17866458359124566529476545682848912883142607690042
                        24219022671055626321111109370544217506941658960408
                        07198403850962455444362981230987879927244284909188
                        84580156166097919133875499200524063689912560717606
                        05886116467109405077541002256983155200055935729725
                        71636269561882670428252483600823257530420752963450"
                        #"\s" "")))

(defn problem-8 []
  (apply max (map (partial reduce *) (partition 13 1 thousand-digits))))

;; Problem 9
;; not done

;; Problem 10
(defn problem-10 [max]
  (reduce + (take-while #(< % max) primes)))

;; Problem 11
(def grid
  [
   [8 2 22 97 38 15 0 40 0 75 4 5 7 78 52 12 50 77 91 8]
   [49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48 4 56 62 0]
   [81 49 31 73 55 79 14 29 93 71 40 67 53 88 30 3 49 13 36 65]
   [52 70 95 23 4 60 11 42 69 24 68 56 1 32 56 71 37 2 36 91]
   [22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80]
   [24 47 32 60 99 3 45 2 44 75 33 53 78 36 84 20 35 17 12 50]
   [32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70]
   [67 26 20 68 2 62 12 20 95 63 94 39 63 8 40 91 66 49 94 21]
   [24 55 58 5 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72]
   [21 36 23 9 75 0 76 44 20 45 35 14 0 61 33 97 34 31 33 95]
   [78 17 53 28 22 75 31 67 15 94 3 80 4 62 16 14 9 53 56 92]
   [16 39 5 42 96 35 31 47 55 58 88 24 0 17 54 24 36 29 85 57]
   [86 56 0 48 35 71 89 7 5 44 44 37 44 60 21 58 51 54 17 58]
   [19 80 81 68 5 94 47 69 28 73 92 13 86 52 17 77 4 89 55 40]
   [4 52 8 83 97 35 99 16 7 97 57 32 16 26 26 79 33 27 98 66]
   [88 36 68 87 57 62 20 72 3 46 33 67 46 55 12 32 63 93 53 69]
   [4 42 16 73 38 25 39 11 24 94 72 18 8 46 29 32 40 62 76 36]
   [20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74 4 36 16]
   [20 73 35 29 78 31 90 1 74 31 49 71 48 86 81 16 23 57 5 54]
   [1 70 54 71 83 51 54 69 16 92 33 48 61 43 52 1 89 19 67 48]])

(def dir {:n  [[0 0] [-1 0] [-2 0] [-3 0]]
          :ne [[0 0] [-1 1] [-2 2] [-3 3]]
          :e  [[0 0] [0 1] [0 2] [0 3]]
          :se [[0 0] [1 1] [2 2] [3 3]]
          :s  [[0 0] [1 0] [2 0] [3 0]]
          :sw [[0 0] [1 -1] [2 -2] [3 -3]]
          :w  [[0 0] [0 -1] [0 -2] [0 -3]]
          :nw [[0 0] [-1 -1] [-2 -2] [-3 -3]]
          })

(defn add-points [p1 p2]
  [(+ (get p1 0) (get p2 0))
   (+ (get p1 1) (get p2 1))])

(defn get-adjacent [grid coord dir]
  (map (fn [step]
         (get-in grid (add-points coord step) 0)) dir))

(defn get-adj-products [grid coord dirs]
  (map (fn [dir]
         (reduce * (get-adjacent grid coord dir))) (vals dirs)))

(defn problem-11 []
  (apply max
        (flatten
         (for [row grid]
           (map (fn [o]
                  (get-adj-products grid
                                    [(.indexOf grid row) (.indexOf row o)]
                                    dir))
                row)))))

;; Problem 12
(defn triangle-num [n]
  (reduce + (range n 0 -1)))

(defn factorization [num]
  (conj (vec (filter #(= (mod num %) 0)
                     (take-while #(<= (* % 2) num) (iterate inc 1))))
        num))

(defn problem-12 [start stop]
  (loop [n start
         triangle (triangle-num n)]
    (if (= (count (factorization triangle)) stop)
      triangle
      (recur (inc n)
             (triangle-num n)))))
