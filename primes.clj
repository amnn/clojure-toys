(ns primes)

(fn p [hi]
  (let [from #(->> % (iterate inc) (drop 1))]
    (if (zero? hi) []
      (let [ps (p (dec hi))]
        (->> (from (or (peek ps) 2))
             (filter (fn [x] (every? #(pos? (mod x %)) ps)))
             first
             (conj ps))))))
