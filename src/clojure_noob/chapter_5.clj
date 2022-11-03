(ns clojure-noob.chapter-5)

; Domain

(defn create-board
  [rows-count]
  (letfn [(create-board-row [row-index]
            (-> row-index
                inc
                (repeat true)
                vec))]
    (->> rows-count
         range
         (map create-board-row)
         vec)))

(defn valid-cell-coords?
  [board [row column]]
  (and (<= 0 row (-> board count dec))
       (<= 0 column row)))

(defn build-cell-modificator
  [cell-value]
  (fn [board cell-coords]
    (when (valid-cell-coords? board cell-coords)
      (assoc-in board cell-coords cell-value))))

(def remove-peg-from-cell (build-cell-modificator false))
(def set-peg-on-cell (build-cell-modificator true))

(defn get-cell-available-moves
  [board [row column :as cell-coords]]
  (letfn [(create-move [target link]
            {:target target :link link})

          (valid-move? [{:keys [target link]}]
            (and (valid-cell-coords? board target)
                 (get-in board link)
                 (not (get-in board target))))]
    (if (and (get-in board cell-coords)
             (valid-cell-coords? board cell-coords))
      (->> [(create-move [row (+ column 2)] [row (inc column)])
            (create-move [row (- column 2)] [row (dec column)])
            (create-move [(+ row 2) column] [(inc row) column])
            (create-move [(+ row 2) (+ column 2)]
                         [(inc row) (inc column)])
            (create-move [(- row 2) column] [(dec row) column])
            (create-move [(- row 2) (- column 2)]
                         [(dec row) (dec column)])]
           (filter valid-move?))
      [])))

(defn move
  [board source target]
  (when-let [move-config (->> (get-cell-available-moves board source)
                              (some #(when (= target (:target %)) %)))]
    (-> board
        (remove-peg-from-cell source)
        (remove-peg-from-cell (:link move-config))
        (set-peg-on-cell target))))

(defn game-over?
  [board]
  (->> (for [row (-> board count range)
             column (-> row inc range)]
         [row column])
       (every? #(->> % (get-cell-available-moves board) empty?))))

; UI

(def cell-coords-separator "-")
(def invalid-input-message "Invalid input format")

(defn print-board
  [board]
  (let [cell-status-labels {true "X" false "O"}
        
        max-cell-status-label-size (->> cell-status-labels
                                        vals
                                        (map count)
                                        (apply max))
        
        rows-count (count board)
        
        max-cell-label-size (->> rows-count
                                 str
                                 count
                                 (* 2)
                                 (+ max-cell-status-label-size
                                    (count cell-coords-separator)))
        
        row-indent-step-size (/ max-cell-label-size 2)]
    (letfn [(get-whitespaces [n] (-> n (repeat " ") clojure.string/join))
            
            (get-cell-label [row column status]
              (let [cell-label (str (inc row)
                                    cell-coords-separator
                                    (inc column)
                                    (cell-status-labels status))
                    
                    indent-size (as-> cell-label $
                                  (count $)
                                  (- max-cell-label-size $)
                                  (/ $ 2))]
                (str (get-whitespaces (Math/ceil indent-size))
                     cell-label
                     (get-whitespaces (Math/floor indent-size)))))
            
            (get-row-string [row-index]
              (let [indent (->> row-index
                                inc
                                (- rows-count)
                                (* row-indent-step-size)
                                get-whitespaces)
                    
                    cells-labels (->> row-index
                                      board
                                      (map-indexed #(get-cell-label row-index %1 %2))
                                      (clojure.string/join " "))]
                (str indent cells-labels)))]
      (println "Here's your board:")
      (dotimes [row-index rows-count]
        (println (get-row-string row-index))))))

(def print-message println)

(defn read-rows-count
  [min max default]
  (letfn [(parse-count [input-string]
            (some-> (and (re-find #"^\s*\d+\s*$" input-string)
                         (Integer/parseInt input-string))
                    (as-> count
                          (when (<= min count max) count))))]
    (printf "How many rows (from %d to %d)? [%d] " min max default)
    (flush)
    (let [input (read-line)]
      (newline)
      (condp apply [input]
        clojure.string/blank? default
        parse-count :>> identity
        (do
          (println invalid-input-message)
          (recur min max default))))))

(defn read-coords
  [message]
  (letfn [(parse-coords [coords-string]
            (->> (clojure.string/split coords-string #"-")
                 (map #(-> % Integer/parseInt dec))
                 vec))]
    (print message)
    (flush)
    (let [input (read-line)]
      (newline)
      (if-let [coords (some-> (re-find #"^\s*(?:\d+-\d+\s+)*\d+-\d+\s*$" input)
                              (clojure.string/split #" ")
                              (->> (map parse-coords)))]
        coords
        (do
          (println invalid-input-message)
          (recur message))))))

; Service

(defn remove-start-peg
  [board]
  (if-let [updated-board (->> (read-coords "Remove which peg (ex. 1-1)? ")
                              first
                              (remove-peg-from-cell board))]
    updated-board
    (do
      (print-message "Wrong cell's coordinates")
      (recur board))))

(defn play
  [board]
  (let [updated-board (->> (read-coords "Move from where to where (ex. 1-1 3-1)? ")
                           (take 2)
                           (apply move board))]
    (cond
      (nil? updated-board)
      (do
        (print-message "Wrong move's configuration")
        (recur board)) 
      
      (game-over? updated-board)
      (print-message "Game is over :)")
      
      :else
      (do
        (print-board updated-board)
        (recur updated-board)))))

(defn start-game
  []
  (letfn [(identity-print-board [board]
            (print-board board)
            board)]
    (print-message "Get ready to play Peg Thing!")
    (-> (read-rows-count 5 10 5)
        create-board
        identity-print-board
        remove-start-peg
        identity-print-board
        play)))

;;; Exercises

; Exercise 1
(defn attr
  [attribute-kw]
  #(get-in % [:attributes attribute-kw]))

; Exercise 2
(defn my-comp
  [& fns]
  (fn [& args]
    (let [[f & rest-fns] (reverse fns)]
      (reduce #(%2 %1)
              (apply f args)
              rest-fns))))

; Exercise 3
(defn my-assoc-in
  [m [k & ks] v]
  (let [value (if (seq ks)
                (my-assoc-in (get m k {}) ks v)
                v)]
    (assoc m k value)))

; Exercise 4
(update-in {:a {:b {:c 1}}}
           [:a :b :c]
           +
           3 5)

; Exercise 5
(defn my-update-in
  [m [k & ks] f & args]
  (let [new-value (if (seq ks)
                    (apply my-update-in
                           (get m k {})
                           ks
                           f
                           args)
                    (apply f (get m k) args))]
    (assoc m k new-value)))
