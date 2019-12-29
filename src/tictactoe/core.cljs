(ns tictactoe.core
  (:require [reagent.core :as reagent :refer [atom]]
            [clojure.spec.alpha :as s]
            [tictactoe.game]
            [tictactoe.reduceboard]))

;; (enable-console-print!)

;; define your app data so that it doesn't get over-written on reload

(defonce my-app-state
  (atom (tictactoe.game/new-game)))

(defn pc-move
  [game]
  {:pre [(s/valid? :tictactoe.game/game game)]}
  (let [next-mv (tictactoe.game/get-next-move game)]
    (if (nil? next-mv)
      game
      (assoc-in game (concat [:tictactoe.game/boards] next-mv) 1))))

(defn play-round
  [game i j k]
  {:pre [(s/valid? :tictactoe.game/game game)]}
  (-> game
      (assoc-in [:tictactoe.game/boards i j k] 1)
      (pc-move)))

(defn blank [game i j k]
  [:rect {:width 0.9
          :height 0.9
          :fill (cond
                  (tictactoe.board/dead? (get-in game [:tictactoe.game/boards i])) "grey"
                  (tictactoe.reduceboard/winning? (:tictactoe.game/boards game) i j k) "green"
                  :else "red")
          :x (+ (* 4 i) j)
          :y k
          :on-click (fn rect-click [e]
                      (if (not (tictactoe.board/dead? (get-in game [:tictactoe.game/boards i])))
                        (swap! my-app-state #(play-round % i j k))))}])

(defn cross [i j k]
  [:g {:stroke  "darkred"
       :stroke-width 0.2
       :stroke-linecap "round"
       :transform
       (str "translate("  (+ 0.5 j (* 4 i)) "," (+ 0.5 k) ")"
            "scale(0.3)")}
   [:line {:x1 -1 :y1 -1 :x2 1 :y2 1}]
   [:line {:x1 1 :y1 -1 :x2 -1 :y2 1}]])

(defn reset-button
  []
  [:input {:type "Button"
           :value "Reset"
           :on-click #(reset! my-app-state (tictactoe.game/new-game))}])

(defn get-scores
  [game]
  (clojure.string/join " " (for [i (:tictactoe.game/boards game)]
                             (tictactoe.board/get-value i))))

(defn tictactoe
  ([]
   (tictactoe @my-app-state))
  ([app-state]
   [:center
    [:h1 "Tic Tac Toe"]
    [:p (reset-button)]
    [:p (get-scores app-state)]
    [:p (str (tictactoe.reduceboard/calc-game (:tictactoe.game/boards app-state)))]
    (into
     [:svg {:view-box (str "0 0 " 13 " " tictactoe.board/board-size)
            :witdth 400
            :height 400}
      (for [i (range (count (:tictactoe.game/boards app-state)))
            j (range tictactoe.board/board-size)
            k (range tictactoe.board/board-size)]
        (case (get-in app-state [:tictactoe.game/boards i j k])
          0 [blank app-state i j k]
          1 [cross i j k]))])]))

(reagent/render-component [tictactoe]
                          (. js/document (getElementById "app")))

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  (reagent/render-component [tictactoe]
                            (. js/document (getElementById "app"))))


