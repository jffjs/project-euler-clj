(ns project-euler-clj.core)

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
