(ns scratch.joy-of-clojure.trees)

(defn xseq [t]
  (when t
    (concat (xseq (:L t)) [(:val t)] (xseq (:R t)))))

(defn xconj
  [{:keys [val L R] :as tree} v]
  (cond (nil? tree) {:val v :L nil :R nil}
        (< v val) {:val val
                   :L (xconj L val)
                   :R R}
        :else {:val val
               :L L
               :R (xconj R val)}))

