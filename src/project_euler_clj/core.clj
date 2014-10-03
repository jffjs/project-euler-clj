(ns project-euler-clj.core)

;; Problem 1
(defn problem-1 []
  (reduce + (filter #(or (= (mod % 3) 0)
                         (= (mod % 5) 0))
                    (range 1 1000))))
