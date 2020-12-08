(ns advent2020.day05)

(require '[clojure.string :as str])
(require '[clojure.java.io :as io])

(defn bin-to-dec [bin]
  (->> (list (seq bin))
       (map #(Integer/parseInt (apply str %) 2))))

(defn get-row [input]
  (as-> (subs input 0 7) tmp
    (str/replace tmp #"F" "0")
    (str/replace tmp #"B" "1")
    (bin-to-dec tmp)
    (Integer. (first tmp))))

(defn get-col [input]
  (as-> (subs input 7) tmp
    (str/replace tmp #"L" "0")
    (str/replace tmp #"R" "1")
    (bin-to-dec tmp)
    (Integer. (first tmp))))

(defn get-id [input]
  (let [row (get-row input)
        col (get-col input)]
    (+ (* row 8) col)))

(defn read-file [filename]
  (with-open [rdr (io/reader filename)]
    (doall (line-seq rdr))))

(defn find-max [filename]
  (apply max
    (for
      [code (read-file filename)]
      (get-id code))))

(defn find-missing [input]
  (for
    [i
     (range
       (apply min input)
       (apply max input))
     :when (not (.contains input i))]
    i))

(println
  (find-max "src/advent2020/input05.txt"))

(println
  (find-missing
    (for
      [code (read-file "src/advent2020/input05.txt")]
      (get-id code))))

