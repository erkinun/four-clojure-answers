(ns scratch.graphics)

(defn f-values [f max-x max-y]
  (for [x (range max-x) y (range max-y)]
    [x y (->
           (f x y)
           (rem 256))]))

(defn clear [g] (.clearRect g 0 0 400 400))

;(defn draw-values [f xs ys]
;  (clear gfx)
;  (.setSize frame (java.awt.Dimension. xs ys))
;  (doseq [[x y v] (f-values f xs ys)]
;    (.setColor gfx (java.awt.Color. v v v))
;    (.fillRect gfx x y 1 1)))