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

; The function is expected to return an INTEGER.
; The function accepts following parameters:
;  1. INTEGER steps
;  2. STRING path
;

(defn countingValleys [steps path]
  (let [path-array (clojure.string/split path #"")]
    (-> (reduce (fn [{:keys [level valleys]} step]
                  (if (= "D" step)
                    {:level (dec level) :valleys valleys}
                    (if (= -1 level)
                      {:level (inc level) :valleys (inc valleys)}
                      {:level (inc level) :valleys valleys})))
                {:level 0 :valleys 0} path-array)
        :valleys)))

; Complete the jumpingOnClouds function below.
; https://www.hackerrank.com/challenges/jumping-on-the-clouds/
(defn jumpingOnClouds [c]
  (loop [
         index 0
         jumps 0
         clouds c]
    (if (>= index (count clouds))
      (dec jumps)
      (if (= 0 (get clouds (+ index 2)))
        (recur (+ index 2) (inc jumps) clouds)
        (recur (+ index 1) (inc jumps) clouds)))))

;; https://www.hackerrank.com/challenges/repeated-string/
(defn repeatedString [s n]
  (let [substr-length (count s)
        required-repeat (-> n (/ substr-length) long)
        part-str (-> (mod n substr-length)
                     (take s)
                     (clojure.string/join))
        a-counter (fn [str] (count (filter
                                     (fn [c] (= "a" c))
                                     (clojure.string/split str #""))))
        part-count (a-counter part-str)
        a-count (a-counter s)]
    (+ part-count (* required-repeat a-count))))

;; https://www.hackerrank.com/challenges/ctci-array-left-rotation/
(defn rotLeft [a d]
  (loop [times d
         [first & rest] a
         second-vec []]
    (if (= 0 times)
      (concat [first] rest second-vec)
      (recur (dec times) rest (conj second-vec first)))))

;; https://www.hackerrank.com/challenges/new-year-chaos/
(defn minimumBribes [q]
  (let [sorted-q (range 1 (-> q count inc))
        bribes (map-indexed (fn [idx itm]
                              (if (> itm (inc idx))
                                (Math/abs
                                  (-
                                    (nth sorted-q idx) itm))
                                0)) q)
        too-chaotic (some (fn [brb] (>= brb 3)) bribes)]
    (if too-chaotic
      (prn "Too chaotic")
      (prn (reduce + bribes)))))


;; https://www.hackerrank.com/challenges/ctci-ransom-note
(defn checkMagazine [magazine note]
  (let [freq-m (frequencies magazine)
        freq-n (frequencies note)
        not-suff? (some (fn [[k v]] (or (nil? (freq-m k)) (> v (freq-m k)))) freq-n)]
    (if not-suff? (print "No") (print "Yes")))
  )

(defn maximumToys [prices k]
  (let [sorted-p (sort prices)]
    (:toys (reduce (fn [state, toy]
                     (if (<= toy (:budget state))
                       (-> state
                           (update :budget (fn [b] (- b toy)))
                           (update :toys inc))
                       state))
                   {:budget k :toys 0}
                   sorted-p))))

(defn countSwaps [a]
  (let [swap (fn [arr i]
               (let [item (nth arr i)
                     next (nth arr (inc i))]
                 (-> arr (assoc i next (inc i) item))))
        {:keys [swaps sorted]} (loop [i 0
                                      arr a
                                      swaps 0
                                      n (count a)]
                                 (if (>= i n)
                                   {:swaps swaps :sorted arr}
                                   (let [{:keys [swaps sorted]} (loop [i 0
                                                                       arr-p arr
                                                                       swaps swaps]
                                                                  (if (>= i (dec n))
                                                                    {:swaps swaps :sorted arr-p}
                                                                    (if (> (nth arr-p i) (nth arr-p (inc i)))
                                                                      (recur (inc i) (swap arr-p i) (inc swaps))
                                                                      (recur (inc i) arr-p swaps))))]
                                     (recur (inc i) sorted swaps n))))]
    (println "Array is sorted in" swaps "swaps.")
    (println "First Element:" (first sorted))
    (println "Last Element:" (last sorted))))

;; you'd normally just use to set and sort again : ) but anyway
;; https://leetcode.com/explore/interview/card/top-interview-questions-easy/92/array/727/
(defn shift-left [arr i]
  (let [arr (vec arr)
        item (nth arr i)
        first-part (subvec arr 0 i)
        rest-part (subvec arr (inc i) (count arr))]
    (concat first-part rest-part [item])))

(defn remove-duplicates [arr]
  (loop [i 0
         last-i (-> arr count dec)
         changed arr]
    (if (= i last-i)
      (subvec (vec changed) 0 (inc i))
      (let [curr (nth changed i)
            next (nth changed (inc i))]
        (if (== curr next)
          (recur i (dec last-i) (shift-left changed (inc i)))
          (recur (inc i) last-i changed))))))

(defn solveMeFirst [x y]
  (+ x y))

(defn compress [string]
  (let [freqs (partition-by identity string)
        mapped (map (fn [freq-list]
                      (let [c (count freq-list)
                            character (first freq-list)]
                        (if (= c 1)
                          (str character)
                          (str character c)))) freqs)]
    (clojure.string/join "" mapped)))

(defn indices [pred coll]
  (keep-indexed #(when (pred %2) %1) coll))

(defn filter-elements [arr filter-count]
  (let [freqs (frequencies arr)
        eligibiles (filter (fn [[k v]] (<= filter-count v)) freqs)
        keys (map first eligibiles)]
    (->> keys
         (map (fn [k] {k (first (indices #(= k %) arr))}))
         (into {})
         (sort-by val <)
         (map first)
         )))

(defn twoStrings [s1 s2]
  (let [set1 (set s1)
        set2 (set s2)
        itrstn (clojure.set/intersection set1 set2)]
    (if (not-empty itrstn)
      "YES"
      "NO"))
  )

;; https://www.hackerrank.com/challenges/ctci-making-anagrams/problem
(defn remove-str [string c]
  (let [index (clojure.string/index-of string c)]
    (if index
      (let [vec-str (vec string)
            first-part (take index vec-str)
            second-part (take-last (- (count vec-str) (inc index)) vec-str)]
        (clojure.string/join "" (concat first-part second-part)))
      string)))

(defn makeAnagram [a b]
  (let [vec-a (vec a)
        vec-b (vec b)
        {:keys [common _]} (reduce
                             (fn [{:keys [common target]} c]
                               (if (clojure.string/includes? target (str c))
                                 {:common (conj common c) :target (remove-str target c)}
                                 {:common common :target target}))
                             {:common [] :target b}
                             a)]
    (+
      (- (count vec-a) (count common))
      (- (count vec-b) (count common)))))

;; https://www.hackerrank.com/challenges/balanced-brackets/
(defn isBalanced [s]
  (loop [brackets s
         stack '()]
    (if (seq brackets)
      (let [ch (first brackets)]
        (cond
          (or
            (= \[ ch)
            (= \{ ch)
            (= \( ch)) (recur (rest brackets) (conj stack ch))
          :else (let [top (peek stack)]
                  (condp = ch
                    \] (if (= \[ top)
                         (recur (rest brackets) (pop stack))
                         "NO")
                    \} (if (= \{ top)
                         (recur (rest brackets) (pop stack))
                         "NO")
                    \) (if (= \( top)
                         (recur (rest brackets) (pop stack))
                         "NO")))))
      (if (seq stack)
        "NO"
        "YES"))))


; https://www.hackerrank.com/challenges/alternating-characters
(defn alternatingCharacters [s]
  (->
    (reduce (fn [{:keys [lead deletions]} c]
              (cond
                (and (= lead \A) (= c \B)) {:lead c :deletions deletions}
                (and (= lead \A) (= c \A)) {:lead c :deletions (inc deletions)}
                (and (= lead \B) (= c \A)) {:lead c :deletions deletions}
                :else {:lead c :deletions (inc deletions)}))
            {:lead (first s) :deletions 0}
            (rest s))
    :deletions))

;; https://www.hackerrank.com/challenges/sherlock-and-valid-string/problem
(defn isValid [s]
  (let [freqs (frequencies s)
        occrs (group-by (fn [[_ v]] v) freqs)
        min (apply min (keys occrs))
        all-keys (keys occrs)
        impossible? (some #(> % (inc min)) all-keys)]
    (cond
      (= (count occrs) 1) "YES"
      (and (not impossible?) (-> occrs (get (inc min)) count (= 1))) "YES"
      (and (= (count occrs) 2) (some #(= 1 %) all-keys) (= 1 (count (get occrs 1)))) "YES"
      :else "NO")
    ))

(defn minimumAbsoluteDifference [arr]
  (let [sorted (sort arr)
        diffs (reduce (fn [{:keys [prev diff-arr]} item]
                        {:prev item :diff-arr (conj diff-arr (Math/abs (- item prev)))})
                      {:prev     (first sorted)
                       :diff-arr []}
                      (rest sorted))]
    (-> diffs :diff-arr sort first)))

;; https://www.hackerrank.com/challenges/luck-balance/problem
(defn luckBalance [k contests]
  (let [important-ones (->> contests
                            (filter (fn [[_ importance]] (= 1 importance)))
                            (sort-by (fn [[luck _]] luck))
                            reverse)
        can-lose (take k important-ones)
        must-win (drop k important-ones)
        rest (filter (fn [[_ importance]] (= 0 importance)) contests)]
    (prn (concat important-ones rest))
    (-
      (reduce + (map (fn [[importance _]] importance) (concat can-lose rest)))
      (reduce + (map (fn [[importance _]] importance) must-win)))))

(defn fizzBuzz [n]
  (loop [i 1]
    (if (> i n)
      nil
      (do
        (cond
          (= 0 (mod i 15)) (println "FizzBuzz")
          (= 0 (mod i 5)) (println "Buzz")
          (= 0 (mod i 3)) (println "Fizz")
          :else (println i))
        (recur (inc i)))))
  )

(defn grid->grid-map [grid]
  (let [spread (map (fn [row] (clojure.string/split row #"")) grid)]
    (:acc (reduce (fn [{:keys [row acc]} item]
                    {:row (inc row)
                     :acc (into acc (:acc-m
                                      (reduce (fn [{:keys [col acc-m]} i] {:col (inc col) :acc-m (into acc-m [[[row col] (Integer/parseInt i)]])})
                                              {:col 0 :acc-m acc}
                                              item)))})
                  {:row 0 :acc {}}
                  spread))))

(defn region [grid-map]
  (let [ones (filter (fn [[k v]] (= v 1)) grid-map)]

    (loop [to-test (keys ones)
           regions #{}]
      (if (seq to-test)
        ;; do your thing
        (let [first-item (first to-test)
              [x y] first-item
              possibles #{[(dec x) y] [(inc x) y] [x (dec y)] [x (inc y)]}
              candidates (rest to-test)
              actuals (filter
                        (fn [point] (contains? possibles point))
                        candidates)
              current-region (conj actuals first-item)]
          (recur (remove (fn [point] (some (fn [item] (= item point)) current-region)) to-test)
                 (conj regions current-region)))
        regions))))

(defn overlaps [gm1 gm2]
  ;; TODO complete and over the two maps
  gm1
  )

(defn countMatches [grid1 grid2]
  (let [gm1 (grid->grid-map grid1)
        gm2 (grid->grid-map grid2)
        overlaps (overlaps gm1 gm2)]
    (count (region overlaps)))
  )

(defn find-region [[x y] region others]
  (let [connections (for [xi [(dec x) x (inc x)]
                          yi [(dec y) y (inc y)]
                          :when (not= [x y] [xi yi])]
                      [xi yi])
        matches (filter (fn [con] (contains? (set others) con)) connections)]
    (if (seq matches)
      (reduce (fn [region match] (clojure.set/union region (find-region match region (remove
                                                                                       (fn [o] (contains? (set matches) o))
                                                                                       others))))
              (into region matches)
              matches)
      (set region))))


;; https://www.hackerrank.com/challenges/connected-cell-in-a-grid/problem
(defn connectedCell [matrix]
  (let [filled-cells (set (for [x (range (count matrix))
                                y (range (count (first matrix)))
                                :when (= 1 (-> matrix (nth x) (nth y)))]
                            [x y]))]
    (loop [fills filled-cells
           regions #{}]
      (if (seq fills)
        (let [first-item (first fills)
              rest-of-them (rest fills)
              region-here (find-region first-item #{first-item} rest-of-them)]
          (recur rest-of-them (conj regions region-here)))
        (do
          (->> regions
               set
               (map #(count %))
               sort
               last))))))

;; https://www.hackerrank.com/challenges/climbing-the-leaderboard/problem
;; TODO create the [score - rank] tuple of vectors
;; TODO then filter and get the last items rank + 1
(defn climbingLeaderboard-easy [ranked player]
  (let [sorted-ranks (->> ranked set (sort-by -))]
    (map
      (fn [play]
        (let [better-scores (filterv #(> % play) sorted-ranks)]
          (inc (count better-scores))))
      player)
    )
  )

;; TODO optimized
(defn climbingLeaderboard [ranked player]
  (let [distinct-ranks (distinct ranked)]
    (:rankings (reduce
                 (fn [{:keys [i rankings]} score]
                   (loop [index i]
                     (if (< score (nth distinct-ranks index))
                       {:i index :rankings (conj rankings (+ 2 index))}
                       (if (= 0 index)
                         {:i index :rankings (conj rankings 1)}
                         (recur (dec index))))))
                 {:i (dec (count distinct-ranks)) :rankings []}
                 player))))

(defn sort-parts [work]
  (lazy-seq
    (loop [[part & parts] work]
      (prn part)
      (prn parts)
      (if-let [[pivot & xs] (seq part)]
        (do
          (let [smaller? #(< % pivot)]
            (recur (list*
                     (filter smaller? xs)
                     pivot
                     (remove smaller? xs)
                     parts))))
        (when-let [[x & parts] parts]
          (cons x (sort-parts parts)))))))

(defn qsort [xs]
  (sort-parts (list xs)))

(defn my-qsort [xs]
  (if (or (not (seq xs)) (= 1 (count xs)))
    xs
    (let [pivot (first xs)
          smaller? #(< % pivot)
          bigger? #(> % pivot)]
      (concat
        (my-qsort (filter smaller? xs))
        [pivot]
        (my-qsort (filter bigger? xs))))))

(defn subsets [seed last-index]
  (let [l (last seed)
        next-not-adjacent (+ 2 l)]
    (if (> next-not-adjacent last-index)
      [seed]
      (let [new-seeds (for [i (range next-not-adjacent (inc last-index))]
                        (conj seed i))]
        (reduce (fn [v n-s] (concat v (subsets n-s last-index))) [seed] new-seeds)))
    ))

(defn all-subsets [arr]
  (let [last-index (-> arr count dec)
        all-indexes (for [i (range 0 (inc last-index))]
                      (subsets [i] last-index))]
    (reduce concat [] all-indexes)))

(defn sum-for-set [arr ss]
  (->> ss
       (map #(nth arr %))
       (reduce +)))

(defn maxSubsetSum-first-try [arr]
  (let [all-ss (all-subsets arr)
        sums (map (partial sum-for-set arr) all-ss)
        max-sum (->> sums
                     (filter #(> % 0))
                     sort
                     reverse
                     first)]
    (if (< max-sum 0)
      0
      max-sum)))

(defn maxSubsetSum [arr]
  (let [zero-max (nth arr 0)
        one-max (nth arr 1)]
    (loop [max-so-far (max zero-max one-max 0)
           max-at-index [zero-max one-max]
           i 2]
      (if (>= i (count arr))
        max-so-far
        (let [val-i (nth arr i)
              max-at-i (max max-so-far val-i (+ val-i (nth max-at-index (- i 2))))]
          (recur max-at-i (conj max-at-index max-at-i) (inc i)))))))


(defn regions [roads]
  (loop [regs []
         rds roads
         road-count 0]
    (if (seq rds)
      (let [[c1 c2] (first rds)
            region-found (some #(when (or
                                        (% c1)
                                        (% c2))
                                  %)
                               regs)]
        (if region-found
          (let [others (filter #(not= % region-found) regs)
                road-not-necessary? (and
                                  (contains? region-found c1)
                                  (contains? region-found c2))]
            (recur
              (conj others (into region-found #{c1 c2}))
              (rest rds)
              (if road-not-necessary? road-count (inc road-count))))
          (recur (conj regs #{c1 c2}) (rest rds) (inc road-count))))
      {:regions regs :road-count road-count})))

(defn roadsAndLibraries [n c_lib c_road cities]
  (if (> c_lib c_road)
    (let [{:keys [regions road-count]} (regions cities)
          connected-cities (->> regions
                                (map count)
                                (reduce +))
          deprived-cities (- n connected-cities)]
      (+ (* c_lib (count regions))
         (* c_road road-count)
         (* c_lib deprived-cities)))
    (* n c_lib)))

;; https://www.hackerrank.com/challenges/sherlock-and-anagrams
(defn substrs [s]
  (reduce concat (for [i (range 0 (count s))]
                   (for [j (range 0 (-> s count (- i)))]
                     (subs s j (+ j i 1))))))

(defn comb-2 [n]
  (-> n
      (* (dec n))
      (/ 2)))

(defn sherlockAndAnagrams [s]
  (let [all-ss (substrs s)
        all-freqs (map frequencies all-ss)
        grps (group-by identity all-freqs)]
    (->> grps
         (map (fn [[k v]] [k (count v)]))
         (filter (fn [[_ v]] (>= v 2)))
         (map (fn [[_ v]] (comb-2 v)))
         (reduce +))))

;; https://www.hackerrank.com/challenges/minimum-swaps-2
(defn minimumSwaps [arr]
  (let [sorted-arr (sort arr)]
    (loop [cur-arr arr
           swap-count 0
           idx 0]
      ;; TODO what if it doesnt match after idx > count of arr?
      (if (= cur-arr sorted-arr)
        swap-count
        (let [cur-indexes (into {} (map-indexed (fn [idx item] [item idx]) cur-arr))
              cur (nth sorted-arr idx)
              current-index (get cur-indexes cur)]
          (if (= idx current-index)
            (recur cur-arr swap-count (inc idx))
            (recur (assoc cur-arr idx cur current-index (nth cur-arr idx)) (inc swap-count) (inc idx))))))))