(ns advent2020.day03)

(require '[clojure.java.io :as io])

(defrecord Rule [right down])

(defn read-file [filename]
  (into []
  (for
    [line (with-open
            [rdr (io/reader filename)]
            (doall (line-seq rdr)))]
    (into [] (seq line)))))

(defn coordinates [rows, columns, rule]
  (loop [col 0
         row 0
         result []]
    (if (<= (+ row (:down rule)) rows)
      (recur
        (+ 
          (if (>= (+ col (:right rule)) columns)
            (- col columns)
            col)
          (:right rule))
        (+ row (:down rule))
        (conj result (vector row col)))
      result)))

(defn count-trees [array rule]
  (let [coords (coordinates
                 (count array)
                 (count (first array))
                 rule)]
    (count (filter
             identity
             (for
               [point coords]
               (if
                (= \# (get
                         (get
                           array
                           (get point 0))
                         (get point 1)))
                true))))))

(defn count-trees-more [array rules]
  (reduce * (for
              [rule rules]
              (count-trees array rule))))

(println 
  (count-trees
    (read-file "src/advent2020/input03.txt")
    (->Rule 3 1)))

(println 
  (count-trees-more 
    (read-file "src/advent2020/input03.txt")
    [(->Rule 1 1)
     (->Rule 3 1)
     (->Rule 5 1)
     (->Rule 7 1)
     (->Rule 1 2)]))
