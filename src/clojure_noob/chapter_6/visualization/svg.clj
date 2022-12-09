(ns clojure-noob.chapter-6.visualization.svg
  (:require [clojure.string :as s])
  (:refer-clojure :rename {max core-max
                           min core-min}))

(defn- latlng->point
  [latlng]
  (str (:lat latlng) "," (:lng latlng)))

(defn- comparator-over-maps
  [comparator-fn ks]
  (fn [maps]
    (zipmap ks
            (map (fn [k] (->> maps
                              (map k)
                              (apply comparator-fn)))
                 ks))))

(def ^:private ks [:lat :lng])

(defn- points
  [locations]
  (->> locations
       (map latlng->point)
       (s/join " ")))

(def ^:private min (comparator-over-maps core-min ks))
(def ^:private max (comparator-over-maps core-max ks))

(defn- translate-to-00
  [locations]
  (let [mincoords (min locations)]
    (map #(merge-with - % mincoords) locations)))

(defn- scale
  [width height locations]
  (let [base-by-property {:lat height :lng width}]
    (-> locations
        max
        (->> (map (fn [[k v]]
                    [k (/ (k base-by-property) v)])))
        (->> (into {}))
        (as-> ratio
              (map #(merge-with * % ratio) locations)))))

(defn- line
  [points]
  (str "<polyline points=\"" points "\" />"))

(defn- transform
  [width height locations]
  (->> locations
       translate-to-00
       (scale width height)))

(defn xml
  [width height locations]
  (str "<svg height=\"" height "\" width=\"" width "\">"
       "<g transform=\"translate(0," height ")\">"
       "<g transform=\"rotate(-90)\">"
       (->> locations
            (transform width height)
            points
            line)
       "</g></g></svg>"))
