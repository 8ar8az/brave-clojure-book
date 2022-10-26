(ns clojure-noob.chapter-3)

(def asym-hobbit-body-parts [{:name "head" :size 3}
                             {:name "left-eye" :size 1}
                             {:name "left-ear" :size 1}
                             {:name "mouth" :size 1}
                             {:name "nose" :size 1}
                             {:name "neck" :size 2}
                             {:name "left-shoulder" :size 3}
                             {:name "left-upper-arm" :size 3}
                             {:name "chest" :size 10}
                             {:name "back" :size 10}
                             {:name "left-forearm" :size 3}
                             {:name "abdomen" :size 6}
                             {:name "left-kidney" :size 1}
                             {:name "left-hand" :size 2}
                             {:name "left-knee" :size 2}
                             {:name "left-thigh" :size 4}
                             {:name "left-lower-leg" :size 3}
                             {:name "left-achilles" :size 1}
                             {:name "left-foot" :size 2}])

(def full-hobbit-body-parts
  (vec (reduce
        (fn [acc {:keys [name size]}]
          (conj acc
                {:name (clojure.string/replace name #"^left-" "right-")
                 :size size}))
        (set asym-hobbit-body-parts)
        asym-hobbit-body-parts)))

(defn hit []
  (let [body-parts-size-sum (reduce + (map :size full-hobbit-body-parts))
        target (rand body-parts-size-sum)]
    (reduce (fn [prev-parts-size-sum part]
              (let [current-with-prev-parts-size-sum (+ prev-parts-size-sum (:size part))]
                (if (> current-with-prev-parts-size-sum target)
                  (reduced part)
                  current-with-prev-parts-size-sum)))
            0
            full-hobbit-body-parts)))

;;; Exercises

; Exercise 1
(str "Hello" ", " "World!")
(vector 1 2 3 4 5)
(list 1 2 3 4 5)
(hash-map :key1 3 :key2 4)
(hash-set 1 1 2 2 3 3 4 4 5 5)

; Exercise 2
(defn add-100
  [n]
  (+ n 100))

; Exercise 3
(defn dec-maker
  [subtrahend]
  (fn [minuend] (- minuend subtrahend)))

; Exercise 4
(defn mapset
  [fn coll]
  (set (map fn coll)))

; Exercise 5
(defn make-alien-body-parts
  [body-parts]
  (let [left-side-part-regex #"^left"]
    (reduce
     (fn [acc part]
       (let [part-name (:name part)]
         (if (re-find left-side-part-regex part-name)
           (into acc (map
                      #(assoc part :name (clojure.string/replace
                                          part-name
                                          left-side-part-regex
                                          (str %)))
                      (range 1 6)))
           (conj acc part))))
     []
     body-parts)))

; Exercise 6
(defn make-arbitrary-count-body-parts
  [body-parts count]
  (let [left-side-part-regex #"^left"]
    (reduce
     (fn [acc part]
       (let [part-name (:name part)]
         (if (re-find left-side-part-regex part-name)
           (into acc (map
                      #(assoc part :name (clojure.string/replace
                                          part-name
                                          left-side-part-regex
                                          (str %)))
                      (range 1 (inc count))))
           (conj acc part))))
     []
     body-parts)))
