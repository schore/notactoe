(ns tictactoe.game
  (:require [tictactoe.board]
            [tictactoe.reduceboard]))

(def number-of-boards 3)

(defn new-game
  ([] (new-game number-of-boards))
  ([n]
   {:boards (into [] (for [i (range n)] tictactoe.board/new-board))
    :player :p1}))

(defn cross-out
  [game i j k]
  (assoc-in game [:boards i j k] 1))

(defn get-playable-boards
  [game]
  (->> (:boards game)
       (map (not tictactoe.board/dead?))))

(defn is-playable?
  [game i]
  (get (get-playable-boards game) i))

(defn possible-moves
  [game]
  (let [all-moves (for [i (range (count (:boards game)))
                        j (range tictactoe.board/board-size)
                        k (range tictactoe.board/board-size)]
                    [i j k])]
    (->> all-moves
         (filter #(not (tictactoe.board/dead? (get-in game [:boards (first %)]))))
         (filter #(= 0 (get-in game (concat [:boards] %)))))))

(defn non-ending-moves
  [game]
  (filter #(not (empty? (possible-moves (assoc-in game (concat [:boards] %) 1)))) (possible-moves game)))

(defn winning-moves
  [game]
  (->> (possible-moves game)
       (filter #(apply (partial tictactoe.reduceboard/winning? (:boards game)) %))))

(defn get-next-move
  "get the next mv, choose an winning move if possible
  In case f is passed, it is possible to choose the element,
  which you wan't to use"
  ([game]
   (get-next-move game rand-int))
  ([game f]
   (let [winning-mv (winning-moves game)
         non-ending-mv (non-ending-moves game)
         mv (possible-moves game)]
     (cond
       (not (empty? winning-mv)) (nth winning-mv (f (count winning-mv)))
       (not (empty? non-ending-mv)) (nth non-ending-mv (f (count non-ending-mv)))
       (not (empty? mv)) (nth mv (f (count mv)))))))
