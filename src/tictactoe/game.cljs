(ns tictactoe.game
  (:require [tictactoe.board]
            [clojure.spec.alpha :as s]))

(def number-of-boards 3)

(s/def ::zero-or-one #{0 1})
(s/def ::board (s/coll-of (s/coll-of ::zero-or-one
                                     :kind vector? :count 3)
                          :kind vector? :count 3))

(s/def ::boards (s/coll-of ::board :kind vector?))
(s/def ::game (s/keys :req [::boards]))

(defn new-game
  ([]
   {:post [(s/valid? ::game %)]}
   (new-game number-of-boards))
  ([n]
   {:post [(s/valid? ::game %)]}
   {::boards (into [] (for [i (range n)] tictactoe.board/new-board))}))

(defn cross-out
  [game i j k]
  {:pre [(s/valid? ::game game)]
   :post [(s/valid? ::game %)]}
  (assoc-in game [::boards i j k] 1))

(defn get-playable-boards
  [game]
  (->> (:boards game)
       (map (not tictactoe.board/dead?))))

(defn is-playable?
  [game i]
  (get (get-playable-boards game) i))

(defn possible-moves
  [game]
  {:pre [(s/valid? ::game game)]}
  (let [all-moves (for [i (range (count (::boards game)))
                        j (range tictactoe.board/board-size)
                        k (range tictactoe.board/board-size)]
                    [i j k])]
    (->> all-moves
         (filter #(not (tictactoe.board/dead? (get-in game [::boards (first %)]))))
         (filter #(= 0 (get-in game (concat [::boards] %)))))))

(defn non-ending-moves
  [game]
  {:pre [(s/valid? ::game game)]}
  (filter #(not (empty? (possible-moves (assoc-in game (concat [::boards] %) 1)))) (possible-moves game)))

(defn winning-moves
  [game]
  {:pre [(s/valid? ::game game)]}
  (->> (possible-moves game)
       (filter #(apply (partial tictactoe.reduceboard/winning? (::boards game)) %))))

(defn get-next-move
  "get the next mv, choose an winning move if possible
  In case f is passed, it is possible to choose the element,
  which you wan't to use"
  ([game]
   {:pre [(s/valid? ::game game)]}
   (get-next-move game rand-int))
  ([game f]
   {:pre [(s/valid? ::game game)]}
   (let [winning-mv (winning-moves game)
         non-ending-mv (non-ending-moves game)
         mv (possible-moves game)]
     (cond
       (not (empty? winning-mv)) (nth winning-mv (f (count winning-mv)))
       (not (empty? non-ending-mv)) (nth non-ending-mv (f (count non-ending-mv)))
       (not (empty? mv)) (nth mv (f (count mv)))))))
