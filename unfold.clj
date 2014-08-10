;; Unfold, like Data.List.unfoldr of Haskell
;; Author: Ashok Menon 10/08/2014
(ns unfold)

(defn unfold
  "`unfold f x` builds a lazy sequence from a seed `x` by applying `f` to it.
  `f` must return a 2-tuple `[y x']` or `nil`, where `y` is the head of the
  generated seq, `x'` is the seed for the rest of the seq, and the absence
  of both results in an empty list."
  [f x]
  (lazy-seq
    (when-let [[y x'] (f x)]
      (cons y (unfold f x')))))
