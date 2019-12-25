(ns tictactoe.board
  (:require [clojure.test.check.generators :as gen]
            [clojure.test.check :as tc]
            [clojure.test.check.properties :as prop]))

(defn rotate-board
  ([board]
   [[(get-in board [2 0]) (get-in board [1 0]) (get-in board [0 0])]
    [(get-in board [2 1]) (get-in board [1 1]) (get-in board [0 1])]
    [(get-in board [2 2]) (get-in board [1 2]) (get-in board [0 2])]])
  ([board n]
   (cond
     (<= n 0) board
     (= n 1) (rotate-board board)
     :else (rotate-board (rotate-board board) (dec n)))))

(defn mirror-board-horizontal
  [board]
  [(get board 2) (get board 1) (get board 0)])

(defn mirror-board-vertical
  [board]
  [[(get-in board [0 2]) (get-in board [0 1]) (get-in board [0 0])]
   [(get-in board [1 2]) (get-in board [1 1]) (get-in board [1 0])]
   [(get-in board [2 2]) (get-in board [2 1]) (get-in board [2 0])]])

(def nonisomorphic-map
  {[[0 0 0] [0 0 0] [0 0 0]] [:c]
   [[1 0 0] [0 0 0] [0 0 0]] []
   [[0 1 0] [0 0 0] [0 0 0]] []
   [[0 0 0] [0 1 0] [0 0 0]] [:c :c]
   [[1 1 0] [0 0 0] [0 0 0]] [:a :d]
   [[1 0 1] [0 0 0] [0 0 0]] [:b]
   [[1 0 0] [0 1 0] [0 0 0]] [:b]
   [[1 0 0] [0 0 1] [0 0 0]] [:b]
   [[1 0 0] [0 0 0] [0 0 1]] [:a]

   [[0 1 0] [1 0 0] [0 0 0]] [:a]
   [[0 1 0] [0 1 0] [0 0 0]] [:b]
   [[0 1 0] [0 0 0] [0 1 0]] [:a]
   [[1 1 1] [0 0 0] [0 0 0]] []
   [[1 1 0] [1 0 0] [0 0 0]] [:b]
   [[1 1 0] [0 1 0] [0 0 0]] [:a :b]
   [[1 1 0] [0 0 1] [0 0 0]] [:d]
   [[1 1 0] [0 0 0] [1 0 0]] [:a]
   [[1 1 0] [0 0 0] [0 1 0]] [:d]

   [[1 1 0] [0 0 0] [0 0 1]] [:d]
   [[1 0 1] [0 1 0] [0 0 0]] [:a]
   [[1 0 1] [0 0 0] [1 0 0]] [:a :b]
   [[1 0 1] [0 0 0] [0 1 0]] [:a]
   [[1 0 0] [0 1 1] [0 0 0]] [:a]
   [[1 0 0] [0 1 0] [0 0 1]] []
   [[1 0 0] [0 0 1] [0 1 0]] []
   [[0 1 0] [1 1 0] [0 0 0]] [:a :b]
   [[0 1 0] [1 0 1] [0 0 0]] [:b]

   [[0 1 0] [0 1 0] [0 1 0]] []
   [[1 1 1] [1 0 0] [0 0 0]] []
   [[1 1 1] [0 1 0] [0 0 0]] []
   [[1 1 1] [0 0 0] [1 0 0]] []
   [[1 1 1] [0 0 0] [0 1 0]] []
   [[1 1 0] [1 1 0] [0 0 0]] [:a]
   [[1 1 0] [1 0 1] [0 0 0]] [:a]
   [[1 1 0] [1 0 0] [0 0 1]] [:a]
   [[1 1 0] [0 1 1] [0 0 0]] [:b]

   [[1 1 0] [0 1 0] [1 0 0]] [:b]
   [[1 1 0] [0 1 0] [0 1 0]] []
   [[1 1 0] [0 1 0] [0 0 1]] []
   [[1 1 0] [0 0 1] [1 0 0]] [:b]
   [[1 1 0] [0 0 1] [0 1 0]] [:a :b]
   [[1 1 0] [0 0 1] [0 0 1]] [:a :b]
   [[1 1 0] [0 0 0] [1 1 0]] [:b]
   [[1 1 0] [0 0 0] [1 0 1]] [:b]
   [[1 1 0] [0 0 0] [0 1 1]] [:a]

   [[1 0 1] [0 1 0] [1 0 0]] []
   [[1 0 1] [0 1 0] [0 1 0]] [:b]
   [[1 0 1] [0 0 0] [1 0 1]] [:a]
   [[1 0 0] [0 1 1] [0 1 0]] [:b]
   [[0 1 0] [1 1 1] [0 0 0]] []
   [[0 1 0] [1 0 1] [0 1 0]] [:a]
   [[1 1 1] [1 1 0] [0 0 0]] []
   [[1 1 1] [1 0 1] [0 0 0]] []
   [[1 1 1] [1 0 0] [1 0 0]] []

   [[1 1 1] [1 0 0] [0 1 0]] []
   [[1 1 1] [1 0 0] [0 0 1]] []
   [[1 1 1] [0 1 0] [1 0 0]] []
   [[1 1 1] [0 1 0] [0 1 0]] []
   [[1 1 1] [0 0 0] [1 1 0]] []
   [[1 1 1] [0 0 0] [1 0 1]] []
   [[1 1 0] [1 1 1] [0 0 0]] []
   [[1 1 0] [1 1 0] [0 0 1]] []
   [[1 1 0] [1 0 1] [0 1 0]] [:b]

   [[1 1 0] [1 0 1] [0 0 1]] [:b]
   [[1 1 0] [0 1 1] [1 0 0]] [:a]
   [[1 1 0] [0 1 1] [0 1 0]] []
   [[1 1 0] [0 1 1] [0 0 1]] []
   [[1 1 0] [0 1 0] [1 1 0]] []
   [[1 1 0] [0 1 0] [1 0 1]] []
   [[1 1 0] [0 1 0] [0 1 1]] []
   [[1 1 0] [0 0 1] [1 1 0]] [:a]
   [[1 1 0] [0 0 1] [1 0 1]] [:a]

   [[1 0 1] [0 1 0] [1 0 1]] []
   [[0 1 0] [1 1 1] [0 1 0]] []
   [[1 1 1] [1 1 1] [0 0 0]] []
   [[1 1 1] [1 1 0] [1 0 0]] []
   [[1 1 1] [1 1 0] [0 1 0]] []
   [[1 1 1] [1 1 0] [0 0 1]] []
   [[1 1 1] [1 0 1] [1 0 0]] []
   [[1 1 1] [1 0 1] [0 1 0]] []
   [[1 1 1] [1 0 0] [1 0 1]] []

   [[1 1 1] [1 0 0] [0 1 1]] []
   [[1 1 1] [0 1 0] [1 1 0]] []
   [[1 1 1] [0 1 0] [1 0 1]] []
   [[1 1 1] [0 0 0] [1 1 1]] []
   [[1 1 0] [1 1 1] [0 1 0]] []
   [[1 1 0] [1 1 1] [0 0 1]] []
   [[1 1 0] [1 0 1] [0 1 1]] [:a]
   [[1 1 0] [0 1 1] [1 1 0]] []
   [[1 1 0] [0 1 1] [1 0 1]] []

   [[1 1 1] [1 1 1] [1 0 0]] []
   [[1 1 1] [1 1 1] [0 1 0]] []
   [[1 1 1] [1 1 0] [1 0 1]] []
   [[1 1 1] [1 1 0] [0 1 1]] []
   [[1 1 1] [1 0 1] [1 1 0]] []
   [[1 1 1] [1 0 1] [1 0 1]] []
   [[1 1 1] [0 1 0] [1 1 1]] []
   [[1 1 0] [1 1 1] [0 1 1]] []
   [[1 1 1] [1 1 1] [1 1 0]] []

   [[1 1 1] [1 1 1] [1 0 1]] []
   [[1 1 1] [1 0 1] [1 1 1]] []
   [[1 1 1] [1 1 1] [1 1 1]] []})

(defn mirror-map
  [input]
  (let [k (first input)
        v (second input)]
    [[k v]
     [(mirror-board-horizontal k) v]
     [(mirror-board-vertical k) v]
     [(mirror-board-vertical (mirror-board-horizontal k)) v]]))


(defn rotate-map
  [input]
  (let [k (first input)
        v (second input)]
    (for [i (range 4)]
      [(rotate-board k i) v])))

(defn complete-map-generator
  [input]
  (->> input
       (mapcat mirror-map)
       (mapcat rotate-map)
       (into {})))

(def complete-map (complete-map-generator nonisomorphic-map))

(defn get-value
  [board]
  (get complete-map board))


(def board-size 3)
;; check for three in a row

(defn collect
  "Collects board-size parameter starting from x y and dx dy"
  [board x y dx dy]
  (for [i (range board-size)]
    (get-in board [(+ x (* i dx)) (+ y (* i dy))])))

(defn three-set?
  [board x y dx dy]
  (every? #(= 1 %)
          (collect board x y dx dy)))

(defn row-win?
  [board]
  (some? (some true?
               (for [i (range board-size)]
                 (three-set? board 0 i 1 0)))))

(defn collumn-win?
  [board]
  (some? (some true?
               (for [i (range board-size)]
                 (three-set? board i 0 0 1)))))

(defn cross-win?
  [board]
  (or (three-set? board 0 0 1 1)
      (three-set? board 0 (dec board-size) 1 -1)))

(defn dead?
  [board]
  (or (row-win? board)
      (collumn-win? board)
      (cross-win? board)))

;; creating board

(def new-board
  (vec (repeat board-size (vec (repeat board-size 0)))))

;;tests
(def my-board-gen
  (gen/vector (gen/vector (gen/choose 0 1) 3) 3))

(gen/sample my-board-gen)

(def rotate-four-times
  (prop/for-all [a  my-board-gen]
                (= a (rotate-board a 4))))
(tc/quick-check 1000 rotate-four-times)

(def mirror-board-vertical-test
  (prop/for-all [a my-board-gen]
                (= a (mirror-board-vertical (mirror-board-vertical a)))))
(tc/quick-check 1000 mirror-board-vertical-test)

(def mirror-board-horizontal-test
  (prop/for-all [a my-board-gen]
                (= a (mirror-board-horizontal (mirror-board-horizontal a)))))
(tc/quick-check 1000 mirror-board-horizontal-test)

(def map-populated
  (prop/for-all [a my-board-gen]
                (some? (get-value a))))

(tc/quick-check 10000 map-populated)
