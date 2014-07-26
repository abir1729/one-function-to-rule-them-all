(ns one-function-to-rule-them-all)

(defn singleton? [coll]
  (= 1 (count coll)))

(defn one-or-zero-elems [coll]
  (or (empty? coll) (singleton? coll)))

(defn concat-elements [a-seq]
  (reduce #(concat %1 %2)
          []
          a-seq))

;(map #(inc %1) [1 2 3])

;(concat-elements [])            ;=> ()
;(concat-elements [[1 2]])       ;=> (1 2)
;(concat-elements [[1 2] [3 4]]) ;=> (1 2 3 4)
;(vec (concat-elements ["abir" "ch"]))

(defn str-cat [a-seq]
  (apply str (if (empty? a-seq)
               ""
               (reduce #(concat %1 " " %2)
                       (first a-seq)
                       (rest a-seq)))))

;(str-cat ["I" "am" "Legend"])
;(str-cat ["I" "am" "back"])    ;=> "I am back"
;(str-cat ["more" " " "space"]) ;=> "more   space"
;(str-cat [])    ;=> ""
;(conj [1 2] 3 [4 5])

;(seq [[1] [2] [3]])

(defn my-interpose [x a-seq]
  (if (one-or-zero-elems a-seq)
    a-seq
    (seq (reduce #(conj %1 x %2)
            [(first a-seq)]
            (rest a-seq)))))

;(my-interpose 0 [1 2 3])
;(my-interpose "," ["I" "me" "myself"]) ;=> ("I" "," "me" "," "myself")
;(my-interpose :a [1])                  ;=> (1)
;(my-interpose :a [])

(defn my-count [a-seq]
  (reduce (fn [count elem]
            (inc count)) 0 a-seq))

;(my-count [])

(defn my-reverse [a-seq]
  (if (one-or-zero-elems a-seq)
    a-seq
    (reduce (fn [curr elem]
              (cons elem curr))
            []
            a-seq)))

;(my-reverse [1 2 3])
;(assoc [1 2] 0 3)

(defn min-max-element [a-seq]
  (loop [min-elem (first a-seq)
         max-elem (first a-seq)
         the-seq a-seq]
    (if (empty? the-seq)
      [min-elem max-elem]
      (let [next-elem (first the-seq)
            rest-seq (rest the-seq)]
        (cond
         (< next-elem min-elem) (recur next-elem max-elem rest-seq)
         (> next-elem max-elem) (recur min-elem next-elem rest-seq)
         :else (recur min-elem max-elem rest-seq))))))

;(min-max-element [1 2 3])

(defn insert [sorted-seq n]
  (let [helper (fn [curr-seq rest-seq]
                 (if (empty? rest-seq)
                   (conj curr-seq n)
                   (let [next-elem (first rest-seq)]
                   (if (<= n next-elem)
                     (concat curr-seq (cons n rest-seq))
                     (recur (conj curr-seq next-elem) (rest rest-seq))))))]
    (helper [] sorted-seq)))

;(insert [1 2 3] 5)
;(insert [] 2)      ;=> (2)
;(insert [1 3 4] 2) ;=> (1 2 3 4)
;(insert [1] 2)     ;=> (1 2)

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

;(insertion-sort [1 2 -1 -0.3])

(defn parity [a-seq]
  [:-])

(defn minus [x]
  :-)

(defn count-params [x]
  :-)

(defn my-* [x]
  :-)

(defn pred-and [x]
  (fn [x] :-))

(defn my-map [f a-seq]
  [:-])
