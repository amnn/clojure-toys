(ns split-every)

(fn [coll n]
  (letfn [(split-every [n coll]
            (let [[f r] (split-at n coll)]
              (cons f (when-let [s (seq r)]
                        (split-at n s)))))]
    (apply map list (split-every n coll))))

;; Equivalently
#(apply map list (partition %2 %1))

;; (split-every defined above is actually partition)
