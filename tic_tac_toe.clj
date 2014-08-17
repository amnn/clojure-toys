;; Tic-Tac-Toe board analyser
(ns tic-tac-toe)

(defn winner [g]
  (let [di1 #(map get % (range))
        di2 #(di1 (reverse %))
        d1 (di1 g) d2 (di2 g)
        g' (apply map vector g)
        win? #(some (partial every? (partial = %)) %2)]
    (condp win? (list* d1 d2 (concat g g'))
      :x :x
      :o :o
      nil)))
