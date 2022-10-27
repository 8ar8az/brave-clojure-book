(ns clojure-noob.chapter-4)

(defn my-map
  [f & colls]
  (seq (reduce
        (fn [acc nth-colls-elements]
          (conj acc (apply f nth-colls-elements)))
        []
        (partition (count colls) (apply interleave colls)))))

(defn my-filter
  [pred coll]
  (seq (reduce
        (fn
          [acc coll-element]
          (if (pred coll-element)
            (conj acc coll-element)
            acc))
        []
        coll)))

(defn my-some
  [pred coll]
  (reduce
   (fn
     [acc coll-element]
     (if-let [pred-result (pred coll-element)]
       (reduced pred-result)
       acc))
   nil
   coll))

(def suspects-file (clojure.java.io/resource "suspects.csv"))

(defn parse
  [string]
  (->> string
       clojure.string/split-lines
       (map (fn [vamp-string]
              (let [[name glitter-index] (clojure.string/split vamp-string #",")]
                {:name name
                 :glitter-index (Integer/parseInt glitter-index)})))
       vec))

(defn glitter-filter
  [min-glitter-index suspects]
  (filter #(>= (:glitter-index %) min-glitter-index) suspects))

;;; Exercises

; Exercise 1
(defn names-of-glitter-filter-result
  [result]
  (map :name result))

; Exercise 2
(defn append
  [suspects new-suspect]
  (conj suspects new-suspect))

; Exercise 3
(defn validate-suspect
  [validation-map suspect]
  (every? (fn [[key validator]]
            (validator (get suspect key)))
          validation-map))

; Exercise 4
(defn stringify
  [suspects-list]
  (->> suspects-list
       (map #(str (:name %) "," (:glitter-index %)))
       (clojure.string/join \newline)))
