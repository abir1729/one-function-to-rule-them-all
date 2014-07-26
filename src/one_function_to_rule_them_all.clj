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
  (reduce (fn [a-set an-elem]
            (if (contains? a-set an-elem)
              (disj a-set an-elem)
              (conj a-set an-elem)))
          #{}
          a-seq))

;(disj #{1 2} 1)
;(conj #{1 2} 3)
;(contains? #{1 2} 2)

;(parity [1 2 3 1 4 4 4])
;(parity [:a :b :c])    ;=> #{:a :b :c}
;(parity [:a :a :b :b]) ;=> #{}
;(parity [1 2 3 1])     ;=> #{2 3}

(defn minus
  ([x] (- 0 x))
  ([x y] (- x y)))

;(minus 2)   ;=> -2
;(minus 4 3) ;=> 1

(defn count-params
  ([] 0)
  ([x & more] (inc (count more))))

;(count-params 1 2 3 4)
;(count-params)            ;=> 0
;(count-params :a)         ;=> 1
;(count-params :a 1 :b :c) ;=> 4

;(reduce * [2 3 4])

;(defn truthy [] true)
;(truthy)

(defn my-*
  ([] 1)
  ([x] x)
  ([x & more] (reduce * x more))
  )

;(apply or [true false])
;(not-any? identity [true false])

;(my-* 1 2)
;(my-*)           ;=> 1
;(my-* 4 3)       ;=> 12
;(my-* 1 2 3 4 5) ;=> 120
(apply vector '(1 2 3))

(defn pred-and
  ([] (fn [_] true))
  ([p] (fn [an-elem] (p an-elem)))
  ([p & predicates]
   (let [all-predicates (cons p predicates)]
     (fn [an-elem]
       (every? identity (map #(% an-elem) all-predicates))))
   ))

;(get [1 2 3] 1)

(defn all-nths [seq-of-seqs n]
  (reduce (fn [curr-seq a-seq]
            (conj curr-seq (get a-seq n)))
          []
          seq-of-seqs))

(defn columns [seq-of-seqs]
  (let [num-cols (count (first seq-of-seqs))
        indices (range 0 num-cols)]
    (reduce (fn [curr-seq curr-index]
              (assoc curr-seq curr-index
                (apply vector (all-nths seq-of-seqs curr-index))))
            []
            indices)
    ))

;(columns [[1 3] [2 5]])

(defn my-map
  ([f a-seq] (reduce (fn [curr-seq elem]
                       (conj curr-seq (f elem)))
                     []
                     a-seq))
  ([f a-seq & more-seqs] (let [all-seqs (cons a-seq more-seqs)
                               arg-lists (columns all-seqs)]
                           (reduce (fn [curr-seq arg-list]
                                     (conj curr-seq (apply f arg-list))) [] arg-lists))))

;(my-map inc [1 2 3 4])                  ;=> (2 3 4 5)
;(my-map + [1 1 1] [1 1 1] [1 1 1])      ;=> (3 3 3)
;(my-map vector [1 2 3] [1 2 3] [1 2 3]) ;=> ((1 1 1) (2 2 2) (3 3 3))

;(filter (pred-and) [1 0 -2])                    ;=> (1 0 -2)
;(filter (pred-and pos? odd?) [1 2 -4 0 6 7 -3]) ;=> (1 7)


