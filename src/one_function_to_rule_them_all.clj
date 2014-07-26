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

(defn min-max-element [a-seq]
  [:-])

(defn insert [sorted-seq n]
  [:-])

(defn insertion-sort [a-seq]
  [:-])

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
