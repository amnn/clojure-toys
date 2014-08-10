(ns sym-list)

;; http://programming-musings.org/2006/02/07/scheme-code-kata/
;; Author: Ashok Menon 5/8/2014

(defn sym-list [coll]
  (reduce (fn [xs y]
            (if (symbol? y)
              ; Init empty field with symbol.
              (conj xs [y []])
              ; Update last sym field, appending new element.
              (update-in xs [(-> xs count dec) 1] #(conj % y))))
          [] coll))

(=
 (sym-list '(a 1 2 3 b 4 5 c d 8 9 e))
 '((a (1 2 3)) (b (4 5)) (c ()) (d (8 9)) (e ())))
