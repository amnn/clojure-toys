;;; Bank OCR Kata
;;; http://codingdojo.org/cgi-bin/index.pl?KataBankOCR
;;;
;;; Author: Ashok Menon, 06/08/2014

(ns bank-ocr
  (:require [clojure.string :as s]))

(def ^:private bit-map {\space 0, \| 1, \_ 1})

(def ^:private digits
  [[(str " _ \n"
         "| |\n"
         "|_|\n") 0]
   [(str "   \n"
         "  |\n"
         "  |\n") 1]
   [(str " _ \n"
         " _|\n"
         "|_ \n") 2]
   [(str " _ \n"
         " _|\n"
         " _|\n") 3]
   [(str "   \n"
         "|_|\n"
         "  |\n") 4]
   [(str " _ \n"
         "|_ \n"
         " _|\n") 5]
   [(str " _ \n"
         "|_ \n"
         "|_|\n") 6]
   [(str " _ \n"
         "  |\n"
         "  |\n") 7]
   [(str " _ \n"
         "|_|\n"
         "|_|\n") 8]
   [(str " _ \n"
         "|_|\n"
         " _|\n") 9]])

(defn- ocr-split-char
  "Split the OCR string into individual characters"
  [ocr-str]
  (->> (s/split ocr-str #"\n")  ; Rows
       (apply map list)         ; Columns
       (partition 3)))          ; Characters

(defn- char->bit-vec
  "A binary vector describing an OCR character"
  [chr] (->> chr (apply concat) (map bit-map)))

(defn- str->bit-vec
  [char-str]
  (->> char-str ocr-split-char first char->bit-vec))

(def ^{:private true
       :doc "Map from bit vectors to integers"}
  bit-vec->int
  (->>
    digits
    (map #(update-in % [0] str->bit-vec))
    (into {})))

(defn- flip
  "Flip between 0 and 1"
  [x] (-> x inc (mod 2)))

(defn- similar-vectors
  "Find vectors with a distance of 1 away from the given vector"
  [bit-vec]
  (when-let [[b & bs] (seq bit-vec)]
    (cons (cons (flip b) bs)
          (map #(cons b %) (similar-vectors bs)))))

(defn- similar-digits
  "Returns digits similar to the binary vector provided"
  [bit-vec]
  (->> bit-vec
       similar-vectors
       (cons bit-vec)
       (map bit-vec->int)
       (filter some?)))

(defn- combine-transpose
  "Takes a seq of seqs `cs` and returns a seq containing every seq derivable by
  picking the the 1st element from the 1st seq in `cs`, the 2nd from the 2nd
  seq in `cs` etc."
  [css]
  (if-let [[cs & css*] (seq css)]
    (let [cbn-trn-rest (combine-transpose css*)]
      (mapcat #(map (partial cons %) cbn-trn-rest) cs))
    '(())))

(defn- similar-accounts
  "Takes the seq of bit-vecs for an acct no. and returns similar numbers"
  [acct]
  (->> acct
       (map similar-digits)
       combine-transpose))

(defn- ocr->vectors
  "Given an OCR String, return the bit vectors that represent its characters"
  [ocr-chrs]
  (->> ocr-chrs
       ocr-split-char
       (map char->bit-vec)))

(defn- clean-parse?
  "Check whether every character was parsed"
  [acct] (every? some? acct))

(defn- failed
  "Replaced nils with ? to present account number"
  [acct] (replace {nil \?} acct))

;;; Public API

(defn check-digit?
  "Check whether the acct no is valid according to check digit"
  [acct]
  (-> (->> acct
           (map * (range 9 0 -1))
           (apply +))
      (mod 11) zero?))

(defn valid-acct-no?
  "Is the account number valid?"
  [acct]
  ((every-pred clean-parse? check-digit?) acct))

(defn parse-ocr
  "Given an OCR string, return the acct no."
  [ocr-chrs]
  (let [bvs (ocr->vectors ocr-chrs)
        attempt (map bit-vec->int bvs)]
    (if (valid-acct-no? attempt)
      attempt
      (if-let [similar (->> bvs
                            similar-accounts
                            (filter valid-acct-no?)
                            seq)]
        (list (failed attempt) :amb similar)
        (cons :ill (failed attempt))))))

;;; Test Cases

(def test-1
  (str "    _  _     _  _  _  _  _ \n"
       "  | _| _||_||_ |_   ||_||_|\n"
       "  ||_  _|  | _||_|  ||_| _|\n"))

(def test-2
  (str " _  _  _  _  _  _  _  _  _ \n"
       "| || || || || || || || || |\n"
       "|_||_||_||_||_||_||_||_||_|\n"))

(def test-3
  (str "    _  _  _  _  _  _     _ \n"
       "|_||_|| || ||_   |  |  | _ \n"
       "  |   |_||_||_|  |  |  | _|\n")
  )

(parse-ocr test-1)
(parse-ocr test-2)
(parse-ocr test-3)
