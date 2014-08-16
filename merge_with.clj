;; merge-with
;; Author: Ashok Menon
(ns merge-with
  (:refer-clojure :exclude [merge-with]))

(defn merge-with
  ([f m n]
   (reduce (fn [m' [k v]]
             (if-let [u (m' k)]
               (assoc m' k (f u v))
               (assoc m' k v)))
           m n))
  ([f m n & etc]
   (reduce #(merge-with f % %2) (list* m n etc))))
