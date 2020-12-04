(ns advent2020.day04)

(require '[clojure.string :as str])

;---------- PREPARE ------------------------------

(defn read-file [filename]
  (for
    [pass (str/split (slurp filename) #"\n\n")]
    (str/replace pass #"\n" " ")))

(defn parse-password [input]
  (apply hash-map
         (flatten
           (for
             [param (str/split input #" ")]
             (str/split param #":")))))

(defn get-passwords-from-file [filename]
  (for
    [pass (read-file filename)]
    (parse-password pass)))

;------------ RULES ------------------------------

(defn rule-is-number [input]
  (if
    (number? input)
    true
    (every? #(Character/isDigit %) input)))

(defn rule-is-alphanumeric [input]
  (=
   (count input)
   (count (re-seq #"[a-zA-Z0-9]" input))))

(defn rule-number-between [input low high]
  (and
    (rule-is-number input)
    (let
      [number (Integer. input)]
      (<= low number high))))

(defn rule-number-digits [input digits]
  (and
    (rule-is-number input)
    (= digits (count input))))

(defn rule-is-in [element collection]
  (some #(= element %) collection))

(defn rule-is-color [input]
  (and
    (= \# (first input))
    (= 7 (count input))
    (rule-is-alphanumeric (subs input 1))))

(defn rule-height [input]
  (let
    [number (Integer. (re-find #"[0-9]*" input))
     unit (str ( re-find #"[a-z]." input))]
    (if
      (= "cm" unit)
      (rule-number-between number 150 193)
      (rule-number-between number 59 76))))

(defn has-required-fields [password required]
  (every? true?
          (for
            [field required]
            (contains? password field))))

(defn password-valid [password]
  (and
    (has-required-fields
      password
      ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"])
    (rule-number-between (get password "byr") 1920 2002)
    (rule-number-between (get password "iyr") 2010 2020)
    (rule-number-between (get password "eyr") 2020 2030)
    (rule-height (get password "hgt"))
    (rule-is-color (get password "hcl"))
    (rule-is-in
      (get password "ecl")
      ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"])
    (rule-number-digits (get password "pid") 9)))

;------------- RESULTS ---------------------------

(defn count-valid1 [passwords fields]
  (count (filter
           identity
           (for
             [password passwords]
             (has-required-fields password fields)))))

(defn count-valid2 [passwords]
  (count (filter
           identity
           (for
             [password passwords]
             (password-valid password)))))

(println
  (count-valid1
    (get-passwords-from-file "src/advent2020/input04.txt")
    ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]))

(println
  (count-valid2
    (get-passwords-from-file "src/advent2020/input04.txt")))

