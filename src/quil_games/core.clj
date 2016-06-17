(ns quil-games.core
  (:require [quil.core :refer :all]
            [quil.middleware :as m])
  (:gen-class))
 
(defn setup []
  {:score 0
   :ball
     {:x 0
      :y 60
      :v 2
      :r 20
      :dir :r}
   :bar 
     {:x (- (/ (width) 2) 5)
      :w 10
      :h 50}})
    
(defn collision-circle-rect [state]
  (if (> (:x (:ball state)) (- (:x (:bar state)) 5))
    (if (< (:x (:ball state)) (+ (:x (:bar state)) 10))
      (if (> (:y (:ball state)) (mouse-y))
        (if (< (:y (:ball state)) (+ (mouse-y) 50))
          true
          false)
        false)
      false)
    false))
    
(defn draw-bar [state]
  (rect (:x (:bar state)) (mouse-y) (:w (:bar state)) (:h (:bar state))))

(defn draw-ball [state]
  (let [ball (:ball state)]
    (ellipse (:x ball) (:y ball) (:r ball) (:r ball))))

(defn reset-ball-x-right [state]
  (assoc-in state [:ball :x] 0))

(defn reset-ball-x-left [state]
  (assoc-in state [:ball :x] (width)))

(defn reset-ball-y [state]
  (assoc-in state [:ball :y] (rand-int (height))))

(defn reset-ball-v [state]
  (assoc-in state [:ball :v] (+ 4 (rand-int 10))))

(defn reset-ball-dir [state]
  (assoc-in state [:ball :dir] (rand-nth [:r :l])))

(defn reset-ball [state]
  (if (= (:dir (:ball state)) :r)
    (-> state
      (reset-ball-x-right)
      (reset-ball-y)
      (reset-ball-v)
      (reset-ball-dir))
    (-> state
      (reset-ball-x-left)
      (reset-ball-y)
      (reset-ball-v)
      (reset-ball-dir))))

(defn update-ball [state]
  (if (= (:dir (:ball state)) :r)
    (if (< (:x (:ball state)) (width)) 
      (update-in state [:ball :x] + (:v (:ball state)))
      (reset-ball state))
    (if (> (:x (:ball state)) 0) 
      (update-in state [:ball :x] - (:v (:ball state)))
      (reset-ball state))))

(defn update-score [state]
  (if (true? (collision-circle-rect state))
    (update-in state [:score] inc)
    state))

(defn update-state [state]
  (-> state
      update-ball
      update-score))

(defn draw [state]
  (if (true? (collision-circle-rect state))
    (background 50 50 50)
    (background 255))
  (fill 50)
  (text-size 30)
  (text (str (:score state)) 30 50)
  (draw-bar state)
  (draw-ball state))

(defsketch example
  :title "Quil games"
  :setup setup
  :draw draw
  :update update-state
  :size [500 400]
  :middleware [m/fun-mode])

(defn -main [& args])

