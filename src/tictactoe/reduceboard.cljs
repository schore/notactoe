(ns tictactoe.reduceboard
  (:require [clojure.test.check.generators :as gen]
            [clojure.test.check :as tc]
            [clojure.test.check.properties :as prop]
            [tictactoe.game]
            [tictactoe.board]))

(def possible-endresults
  [[]
   [:a]
   [:b]
   [:a :b]
   [:b :b]
   [:a :b :b]
   [:c]
   [:a :c]
   [:b :c]
   [:a :b :c]
   [:c :c]
   [:a :c :c]
   [:b :c :c]
   [:a :b :c :c]
   [:d]
   [:a :d]
   [:b :d]
   [:a :b :d]])

(defn tranfsform-table
  "transforms the input list into an record, which contains the number
  of elements"
  [in]
  {:a (count (filter (partial = :a) in))
   :b (count (filter (partial = :b) in))
   :c (count (filter (partial = :c) in))
   :d (count (filter (partial = :d) in))})

(defn reduce-table
  "Reduces a complex input to a simpler form"
  [input]
  (cond
    (>= (:a input) 2) (update input :a #(- % 2))
    (>= (:b input) 3) (update input :b #(- % 2))
    (and (>= (:b input) 2)
         (>= (:c input) 1)) (update input :b #(- % 2))
    (>= (:c input) 3) (-> input
                          (update :a inc)
                          (update :c dec))
    (and (>= (:b input) 2)
         (>= (:d input) 1)) (update input :b #(- % 2))
    (and (>= (:c input) 1)
         (>= (:d input) 1)) (-> input
                                (update :a inc)
                                (update :c dec))
    (>= (:d input) 2) (-> input
                          (update :c (partial + 2))
                          (update :d #(- % 2)))
    :else input))

(defn reduce-board
  "reduces teh game to it's simplest form"
  [board]
  (let [newboard (reduce-table board)]
    (if (= newboard board)
      board
      (recur newboard))))



(defn calc-game
  ([board]
   (->> board
        (map tictactoe.board/get-value)
        (flatten)
        (tranfsform-table)
        (reduce-board)))
  ([board i j k]
   (-> board
       (assoc-in [i j k] 1)
       (calc-game))))

(def winning-endresults
  (map tranfsform-table [[:a]
                         [:b :b]
                         [:b :c]
                         [:c :c]]))



(defn winning?
  ([board]
  (some (partial = (calc-game board))
        winning-endresults))
  ([board i j k]
   (-> board
       (assoc-in [i j k] 1)
       (winning?))))



;; Test function

(def gen-boards (gen/hash-map
                 :a gen/nat
                 :b gen/nat
                 :c gen/nat
                 :d gen/nat))

(def reduce-board-test
  (prop/for-all [a gen-boards]
                (some #(= (reduce-board a) %) (map tranfsform-table possible-endresults))))

(tc/quick-check 1000 reduce-board-test)

