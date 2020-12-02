(ns advent2020.day02)

(require '[clojure.string :as str])
(require '[clojure.java.io :as io])

(defrecord Password [letter rule-one rule-two password])

(defn str-to-pass [string]
  (let [parsed (str/split string #" ")
        numbers (str/split (get parsed 0) #"-")]
    (->Password (str/replace (get parsed 1) #":" "")
                (Integer. (get numbers 0))
                (Integer. (get numbers 1))
                (get parsed 2))))

(defn is-valid1 [password]
  (if (<=
        (:rule-one password)
        (count (re-seq (re-pattern (:letter password)) (:password password)))
        (:rule-two password))
    true))

(defn xor [a b]
  (and (or a b) (not (and a b))))

(defn is-valid2 [password]
  (if (xor
        (= (:letter password) (str (get (:password password) (- (:rule-one password) 1))))
        (= (:letter password) (str (get (:password password) (- (:rule-two password) 1)))))
    true))

(defn read-file [filename]
  (with-open [rdr (io/reader filename)]
    (doall (line-seq rdr))))

(defn count-valid-in-file [filename, validation]
  (count (filter identity
                 (for
                   [line (read-file filename)]
                   (validation (str-to-pass line))))))

(def filename "src/advent2020/input02.txt")
(println
  (count-valid-in-file filename is-valid1)
  (count-valid-in-file filename is-valid2))

