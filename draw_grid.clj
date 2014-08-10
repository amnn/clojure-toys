;; Joy of Clojure Ch. 3
;; "Experimenting with Graphics"

;; Author: Ashok Menon, 9/8/2014

(ns draw-grid
  (:import [java.awt Frame Dimension Color]))

(defn- f-grid
  "Take a binary function `f`, width `w`, height `h` and produce a w by h grid
  `g` of values s.t. `(= (get-in g [i j]) [i j (f i j)])`"
  [f w h]
  (for [x (range w) y (range h)]
    [x y (mod (f x y) 256)]))

(defn draw-values
  "Set the height of Frame `fr` to `[w h]`, clear it, and draw the f-grid of
  `f` in it"
  [fr f w h]
  (.setSize fr (Dimension. w h))
  (let [g (.getGraphics fr)]
    (.clearRect g 0 0 w h)
    (doseq [[x y v] (f-grid f w h)]
      (doto g
        (.setColor (Color. v v v))
        (.fillRect x y 1 1)))))
