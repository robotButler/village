(ns village.world)


(def state {:hour 0})
(def night-hours {:start 18 :end 6})
(def productivity-factors {:day 8.0 :night 4.0})
(def rest-factors {:day (* 8.0 0.7) :night 8.0})




(defn hour-of-day []
  (mod (:hour state) 24))


(defn night?
  []
  (or
   (> (hour-of-day) (:start night-hours))
   (< (hour-of-day) (:end night-hours))))

(defn increment-hour []
  (def state (assoc state :hour (inc (:hour state)))))








