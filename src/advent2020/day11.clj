(ns advent2020.day11)

(require '[clojure.string :as str])

(def FILENAME "src/advent2020/input11.txt")
(def NEIGHBORS1 4)
(def NEIGHBORS2 5)

(def input-file (slurp FILENAME))

(def size
  (let [lines (str/split-lines input-file)]
    [(count lines) (count (first lines))]))

(def seat-map
  (let [[rows cols] size]
    (->> (re-seq #"." input-file)
         (keep-indexed #(if (not= %2 ".")
                          [[(mod %1 cols) (quot %1 cols)] %2]))
         (into {}))))

(def seat-keys (set (keys seat-map)))

(defn neighbors-adjacent [_ location]
  (let [[rows cols] size]
    (for [q [-1 0 1]
          w [-1 0 1]
          :let [[x y :as xy] (map + location [q w])]
          :when (and (not= q w 0)
                     (<= 0 x (dec cols))
                     (<= 0 y (dec rows))
                     (seat-keys xy))]
      xy)))

(defn neighbors-first-sight [seat-map location]
  (let [[rows cols] size]
    (for [q [-1 0 1]
          w [-1 0 1]
          :when (not= q w 0)
          :let [neighbor (->> (rest (range))
                       (keep #(let [[x y :as xy] (->> (map * [q w] [% %])
                                                      (mapv + location))
                                    seat (seat-map xy ".")]
                                (if (and (<= 0 x (dec cols))
                                         (<= 0 y (dec rows)))
                                  (if (#{"#" "L"} seat) xy nil)
                                 -1)))
                       first)]
          :when (not= -1 neighbor)]
      neighbor)))

(defn final-loop [[neighbor-func neighbor-max]]
    (loop [seat-map seat-map]
      (let [occupied (->> seat-keys
                          (filter #(= "#" (seat-map %)))
                          (mapcat #(neighbor-func seat-map %))
                          frequencies)
            changed (keep #(let [cnt (occupied % 0)]
                             (cond (and (= "L" (seat-map %)) (= cnt 0)) [% "#"]
                                   (and (= "#" (seat-map %)) (<= neighbor-max cnt)) [% "L"]))
                          seat-keys)]
        (if (seq changed)
          (recur (into seat-map changed))
          (-> seat-map
              vals
              frequencies
              (get "#"))))))

(pmap final-loop [[neighbors-adjacent NEIGHBORS1]
                  [neighbors-first-sight NEIGHBORS2]])
