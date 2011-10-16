(use '[clojure.contrib.combinatorics :only (permutations)])

(defn all-possible-placements [n]
  "Return a lazy-sequence of placement vectors for all possible arrangements
   of N queens on a chessboard of size N x N."
  (permutations (range 1 (inc n))))

(defn no-duplicates [vec]
  "Returns true if the given vector does not contain duplicate elements, else false."
  (= (count vec) (count (set vec))))

(defn possible-placements [n]
  "Return a lazy-sequence of placement vectors for the possible arrangements
   of N queens which do not allow capture across the same row."
  (filter no-duplicates (all-possible-placements n)))

(defn backslash-diagonal-index [n [col row]]
  "Compute the index of the 'backslash' diagonal for the given
   row and column index."
  (- (+ n row) col))

(defn slash-diagonal-index [[col row]]
  "Compute the index of the 'forward slash' diagonal for the given
   row and column index."
  (+ row col -1))

(defn test-diagonals [n col-row-vec]
  "Test whether each [column, row] coordinate occupies a separate
   forward and backward diagonal from every other pair in the vector"
  (and (no-duplicates (map slash-diagonal-index col-row-vec))
       (no-duplicates
         (map (partial backslash-diagonal-index n) col-row-vec))))

(defn one-indexed [s]
  "Returns a lazy sequence of [index, item] pairs, where items come
   from 's' and indexes count up from one."
  (map vector (iterate inc 1) s))

(defn solution-placement [n placement]
  "Return true if the given placement vector is a solution vector
   for N queens, else false."
  (let [col-row-vec (one-indexed placement)]
    (if (test-diagonals n col-row-vec) true false)))

(defn nqueens [n]
  "Return a vector of all non-capturing placements of N queens on a
   chessboard of size N x N."
  (filter (partial solution-placement n) (possible-placements n)))
