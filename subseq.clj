(ns subseq)

;;; Longest Increasing Subsequence

#(letfn [(max-seq [[as bs]] (if (< (count as) (count bs)) bs as))]
   (when-let
     [lis (max-seq (reduce
                     (fn [[xs' xs :as xss] x]
                       (if (and (seq xs) (> x (peek xs)))
                         [xs' (conj xs x)]
                         [(max-seq xss) [x]]))
                     [[] []] %))]
     (if (< 1 (count lis)) lis [])))
