(use '[clojure.contrib.combinatorics :only (permutations)])

(defn nqueens [n]
  "Return a vector of all non-capturing placements of N queens on a chessboard of size N x N."
  (let [cols (range n)]
    (filter
      #(if (= n (count (set (map + % cols))) (count (set (map - % cols)))) % false)
      (permutations cols))))
