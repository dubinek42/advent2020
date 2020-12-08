(ns advent2020.day06)

(require '[clojure.string :as str])

(defn read-file [filename]
  (for
    [group (str/split (slurp filename) #"\n\n")]
    (str/replace group #"\n" "")))

(defn read-file2 [filename]
  (for
    [group (str/split (slurp filename) #"\n\n")]
    (str/split group #"\n")))

(defn count-distinct [group]
  (count (distinct group)))

(defn count-common [answers]
  (let [person (first answers)
        the-rest (rest answers)]
    (count (filter identity
                   (for
                     [letter person]
                     (every? true?
                             (for
                               [another the-rest]
                               (str/includes? another (str letter)))))))))

(defn the-sum [groups method]
  (reduce
    +
    (for
      [group groups]
      (method group))))

(println
  (the-sum
    (read-file "src/advent2020/input06.txt")
    count-distinct))

(println
  (the-sum
    (read-file2 "src/advent2020/input06.txt")
    count-common))
