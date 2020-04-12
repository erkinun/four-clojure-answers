(ns scratch.solutions)

(defn my-max
  [f & others]
  (let [items (concat [f] others)]
    (loop [max 0
           is items]
      (let [c (count is)
            f (first is)]
        (if (> c 0)
          (if (> f max)
            (recur f (rest is))
            (recur max (rest is)))
          max)))))

(comment (map inc [1 2 3 4 5]))

(map inc [1 2])

(defn create-map [value keys]
  (->>
    (map (fn [k] (vector k value)) keys)
    (into (sorted-map))))

(defn my-last [items]
  (let [c (count items)]
    (if (= 1 c)
      (first items)
      (my-last (rest items)))))


(defn my-penultimate [items]
  (let [c (count items)]
    (if (= 2 c)
      (first items)
      (recur (rest items)))))

(defn my-nth [items index]
  (if (= index 0)
    (first items)
    (recur (rest items) (- index 1))))

(defn my-count [items]
  (loop [index 0
         is items]
    (if (empty? is)
      index
      (recur (+ 1 index) (rest is)))))

(defn sum [items]
  (reduce + items))

(comment
  (+ 1 2 3))

(+ 1 2 3)

(defn my-reverse
  [items]
  (->> items
       (reduce (fn [i rest] (into [rest] [i])))
       flatten))

(defn my-reverse-v2
  [items]
  (let [start []]
    (loop [col start r items]
      (if (not (empty? r))
        (recur (into [(first r)] col) (rest r))
        col)
      )))

(defn fib [n]
  (case n
    1 1
    2 1
    (+ (fib (- n 1)) (fib (- n 2)))))

(defn fib-series [xth]
  (let [start [1 1]]
    (loop [ith 2 cur start]
      (if (>= ith xth)
        (take xth cur)
        (let [ith-min-1 (last cur)
              ith-min-2 (last (drop-last cur))]
          (recur (inc ith) (conj cur (+ ith-min-1 ith-min-2))))))))

(defn is-palindrome? [word]
  (let [r (->> word seq reverse)
        s (seq word)]
    (= s r)))

(defn get-caps [word]
  (->> word
       (filter #(Character/isUpperCase %))
       (clojure.string/join)))

(defn compress [coll]
  (loop [compressed '()
         [f & rest] coll]
    (if (not (nil? f))
      (if (not= f (first compressed))
        (recur (conj compressed f) rest)
        (recur compressed rest))
      (reverse compressed))))


(defn pack-seq [col]
  (loop [xs col
         packed '()]
    (if-let [head (first xs)]
      (if-let [phead (first packed)]
        (if (some #(= head %) phead)
          (recur (rest xs) (conj (rest packed) (conj phead head))) ; phead has head, so add that to the list and recur
          (recur (rest xs) (conj packed (list head))))      ; phead does not have head, so insert it as a new list and recur
        (recur (rest xs) (conj packed (list head))))        ; packed is empty, insert head as a list
      (reverse packed))))                                   ; xs is empty return packed

(defn half-truth [b & bs]
  (let [booleans (conj bs b)
        length (count booleans)
        true-length (count (filter #(= true %) booleans))]
    (cond
      (= 0 true-length) false
      (= length true-length) false
      :else true)))

(defn make-map [keys values]
  (loop [ks keys
         vs values
         generated {}]
    (if (and (not-empty ks) (not-empty vs))
      (recur (rest ks) (rest vs) (assoc generated (first ks) (first vs)))
      generated)))

(defn re-iterate [func value]
  (cons value (lazy-seq (re-iterate func (func value)))))   ; my point : )

(defn my-group-by [func col]
  (loop [r col
         result-map {}]
    (if (not-empty r)
      (let [head (first r)                                  ; as long as we have more items
            applied (func head)]
        (if-let [existing (result-map applied)]             ; if we already have a list of vector for this key
          (recur (rest r) (->> head (conj existing) (assoc result-map applied))) ; add the r to this vector and assoc it again
          (recur (rest r) (assoc result-map applied [head])) ; create a new key value pair and recur
          ))
      result-map)                                           ; return the result if there are not more items on r
    ))


(comment (+ 1 2))

(defn gcd [x y]
  (loop [left (if (>= x y) x y)
         right (if (<= x y) x y)]
    (let [mod-result (mod left right)]
      (if (= mod-result 0)
        right
        (recur right mod-result)))))

(defn my-juxt [& funcs]
  (fn [& args]
    (map #(apply % args) funcs)))

(defn set-intersect [left right]
  (loop [index-set left
         common #{}]
    (if-let [head (first index-set)]
      (recur (rest index-set) (if (contains? right head) (conj common head) common))
      common)))

(defn power-maker [n]
  (fn [x] (loop [start 0
                 acc 1]
            (if (= start n)
              acc
              (recur (inc start) (* x acc))))))

(defn comparisons [operator left right]
  (if (not (operator left right))
    (if (not (operator right left))
      :eq
      :gt)
    :lt))

(defn cartesian [left right]
  (set (for [x left y right] [x y])))

(defn symmetric-diff [left right]
  (let [union (into left right)
        common (clojure.set/intersection left right)]
    (clojure.set/difference union common)))

(defn my-partition [size col]
  (loop [remaining col
         parts []]
    (if (or (empty? remaining) (< (count remaining) size))
      parts
      (recur (drop size remaining) (conj parts (take size remaining))))))

(defn flip [fun]
  (fn [x y]
    (fun y x)))

(defn dot-product [left right]
  (let [zipped (map vector left right)
        multiplied (map #(let [f (first %)
                               l (last %)]
                           (* f l)) zipped)]
    (reduce + multiplied)))

(defn binary-to-int [bin-str]
  (let [arr (map #(if (= \1 %) 1 0) (reverse bin-str))]
    (loop [power 0 acc 0 digits arr]
      (if (empty? digits)
        (int acc)
        (recur (inc power) (+ acc (* (first digits) (Math/pow 2 power))) (rest digits))))))

(defn infix-calculator [& args]
  (loop [acc (first args)
         r (rest args)]
    (if (empty? r)
      acc
      (recur (let [next (first r)]
               (next acc (second r))) (rest (rest r))))))

(defn with-index [col]
  (loop [i 0
         items col
         zipped []]
    (if (empty? items)
      zipped
      (recur (inc i) (rest items) (conj zipped [(first items) i])))))

(def evolve (fn [row] (if (= 1 (count row))
                        [1 1]
                        (loop [index 0
                               new-row [1]]
                          (if (= index (- (count row) 1))
                            (conj new-row 1)
                            (recur (inc index) (conj new-row (+ (row index) (row (inc index))))))))))

(defn pascal-triangle [n]
  (let [evolve-fn (fn [row] (if (= 1 (count row))
                              [1 1]
                              (loop [index 0
                                     new-row [1]]
                                (if (= index (- (count row) 1))
                                  (conj new-row 1)
                                  (recur (inc index) (conj new-row (+ (row index) (row (inc index)))))))))]
    (loop [i 1
           current-row [1]]
      (if (= n i)
        current-row
        (recur (inc i) (evolve-fn current-row))))))

(defn my-map [f [x & xs]]
  (if x
    (lazy-seq
      (cons (f x) (my-map f xs)))
    []))

(defn global-take-while [nth p col]
  (loop [n nth
         acc []
         items col]
    (if (or (empty? items) (= 0 n))
      (drop-last acc)
      (if-let [_ (p (first items))]
        (recur (dec n) (conj acc (first items)) (rest items))
        (recur n (conj acc (first items)) (rest items))))))

(defn balance [n]
  (let [str-n (str n)
        length (count str-n)
        mid (int (/ length 2))]
    (if (>= mid 1)
      (let [char->int #(- (int %) 48)
            left (take mid str-n)
            left-ints (map char->int left)
            right (take-last mid str-n)
            right-ints (map char->int right)
            left-sum (reduce + left-ints)
            right-sum (reduce + right-ints)]
        (= left-sum right-sum))
      true)))

(defn is-bst? [tree]
  (cond
    (nil? tree) true
    (boolean? tree) false
    :else (let [[value left right & others] tree
                should-be-three (= 3 (count tree))
                is-head-ok? (not (nil? value))
                are-there-others? (nil? others)]
            (and is-head-ok? should-be-three are-there-others? (is-bst? left) (is-bst? right)))))

(defn mirror [tree]
  (cond
    (nil? tree) tree
    :else
    (let [[root left right] tree]
      (if (= nil root)
        nil
        [root (mirror right) (mirror left)]))))

(defn eq-tree [left right]
  (cond
    (and (nil? left) (nil? right)) true
    (or (nil? left) (nil? right)) false
    :else
    (let [[lroot lleft lright] left
          [rroot rleft rright] right]
      (and (= lroot rroot) (eq-tree lleft rleft) (eq-tree lright rright)))))

(defn is-symmetric-tree [tree]
  (cond
    (nil? tree) true
    :else (let [[_ left right] tree
                mirrored (mirror left)]
            (eq-tree mirrored right))))

(defn is-symmetric-bundled [tree]
  (letfn [(mirror [tree]
            (cond
              (nil? tree) tree
              :else
              (let [[root left right] tree]
                (if (= nil root)
                  nil
                  [root (mirror right) (mirror left)]))))
          (eq-tree [left right]
            (cond
              (and (nil? left) (nil? right)) true
              (or (nil? left) (nil? right)) false
              :else
              (let [[lroot lleft lright] left
                    [rroot rleft rright] right]
                (and (= lroot rroot) (eq-tree lleft rleft) (eq-tree lright rright)))))]
    (cond
      (nil? tree) true
      :else (let [[_ left right] tree
                  mirrored (mirror left)]
              (eq-tree mirrored right)))))

(defn smaller-than-sq-sums [ints]
  (letfn [(sqrs [digits] (map #(* % %) digits))
          (digits [num] (->> num str vec (map #(- (int %) 48))))
          (sum-nums [nums] (reduce + 0 nums))]
    (count (filter #(< % (sum-nums (sqrs (digits %)))) ints))))

(defn suit->map [suit-str]
  (let [suits {"S" :spade "H" :heart "D" :diamond "C" :club}
        ranks {"2" 0 "3" 1 "4" 2 "5" 3 "6" 4 "7" 5 "8" 6 "9" 7 "T" 8 "J" 9 "Q" 10 "K" 11 "A" 12}
        [suit & rank] suit-str
        s-str (str suit)
        r-str (clojure.string/join rank)]
    {:suit (get suits s-str) :rank (get ranks r-str)}))