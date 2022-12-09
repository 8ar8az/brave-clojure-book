(ns clojure-noob.chapter-6.core
  (:require [clojure-noob.chapter-6.visualization.svg :as svg]
            [clojure.java.browse :as browse])
  (:gen-class))

(def heists [{:location    "Cologne, Germany"
              :cheese-name "Archbishop Hildebold's Cheese Pretzel"
              :lat         50.95
              :lng         6.97}
             {:location    "Zurich, Switzerland"
              :cheese-name "The Standard Emmental"
              :lat         47.37
              :lng         8.55}
             {:location    "Marseille, France"
              :cheese-name "Le Fromage de Cosquer"
              :lat         43.30
              :lng         5.37}
             {:location    "Zurich, Switzerland"
              :cheese-name "The Lesser Emmental"
              :lat         47.37
              :lng         8.55}
             {:location    "Vatican City"
              :cheese-name "The Cheese of Turin"
              :lat         41.90
              :lng         12.45}])

(defn show-map
  []
  (let [filename "map.html"]
    (->> heists
         (svg/xml 50 100)
         (str "<style>polyline { fill:none; stroke:#5881d8; stroke-width:3}</style>")
         (spit filename))
    (->> filename
         (str "file:///"
              (System/getProperty "user.dir")
              "/")
         browse/browse-url)))