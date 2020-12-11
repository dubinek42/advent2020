(ns advent2020.day07)

(require '[clojure.string :as str])
(require '[clojure.java.io :as io])

(def FILENAME "src/advent2020/input07.txt")
(def BAG "shiny gold")

(defn read-file [filename]
  (with-open [rdr (io/reader filename)]
    (doall (line-seq rdr))))

(defn parse-line [line method]
  (let [first-split (str/split line #" bags contain ")]
    [(first first-split)
     (into [] (flatten ( into []
           (for [contain (str/split (first (next first-split)) #", ")]
             (method contain)))))]))

(defn without-number [contain]
  (let [second-split (str/split contain #" ")]
    (str/join " " (subvec second-split 1 (- (count second-split) 1)))))

(defn with-number [contain]
  (flatten
  (let [second-split (str/split contain #" ")]
    [(str/join " " (subvec second-split 1 (- (count second-split) 1)))
     (let [number (first second-split)] (if (= number "no") 0 (Integer. number)))])))

(defn compose-bags-tree [method]
  (apply hash-map
         (apply concat
                (for [line (read-file FILENAME)]
                  (parse-line line method)))))

(defn search-path [root tree]
  (let [branch (get tree root)]
    (if
      (.contains branch BAG)
      true
      (some true? (for [node branch
                        :when (contains? tree node)]
                    (search-path node tree))))))

(defn count-paths [tree]
  (count (filter identity
                (for [[k _] tree]
                  (search-path k tree)))))

(defn count-bags [tree value]
  (if
    (contains? tree value)
    (let [branch (apply hash-map (get tree value))]
      (if
        (contains? branch "other")
        0
        (reduce + (for [[k v] branch]
                    (+ v (* v (count-bags tree k)))))))
    0))

(println
  (count-paths (compose-bags-tree without-number))
  (count-bags (compose-bags-tree with-number) BAG))
