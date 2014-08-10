(ns rotate)

(defn rotate [n coll]
  (cond (neg? n) (->> coll reverse (rotate (- n) coll))
        (pos? n) (let [n* (mod n (count coll))]
                   (concat (drop n* coll) (take n* coll)))))

;; Equivalent (mod will always return a positive result).

#(let [n (mod %1 (count %2))]
  (concat (drop n coll)
          (take n coll)))
