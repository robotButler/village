(ns village.persons)
(require '[village.world :as world])
(use '(clojure.pprint))
(use '(incanter core stats datasets))

(def work-fatigue-factor 8.0)

(def persons [{:rest 25.0 :action :work :money 0.0 :history []} {:rest 60.0 :action :sleep :money 0.0 :history []}])



(defn current-rest-factor
  []
  (if (world/night?)
    (:night world/rest-factors)
    (:day world/rest-factors)))

(defn current-productivity
  [person]
  (* 
   (/ (:rest person) 100.0)
  	(if (world/night?)
    	(:night world/productivity-factors)
    	(:day world/productivity-factors))))


(defn predict-var
  [person action dependent-var]
  (let [action-hist (filter #(= (:action %) action) (:history person))]
    (if (< (count action-hist) 9)
      (mean (map #(dependent-var %) action-hist))
  	(let [dependents (map #(dependent-var %) action-hist)]
      (if (every? #(== 0 %) dependents) 0.0 
    	(let [nights (map #(if (:night? %) 0 100) action-hist)]
      	(let [restfulness (map #(:rest %) action-hist)]
          (let [m (trans (matrix [dependents nights restfulness]))]
            (def x (sel m :cols (range 1 3)))
            (def y (sel m :cols 0))
            ;(println action)
            ;(println dependent-var)
            ;(println action)
            ;(println dependent-var)
            ;(print x)
            ;(print y)
            (def model (linear-model y x))
            (first (predict model [(if (:night? person) 0 100) (:rest person)]))))))))))
    
(defn predict-all
  [person action]
      (let [predicted-money-delta (predict-var person action :money-delta)]
        (let [predicted-rest-delta (predict-var person action :rest-delta)]
          (+ predicted-money-delta predicted-rest-delta))))
          ;(println (str "money delta" predicted-money-delta))
          ;(println (str "rest delta" predicted-rest-delta))
         
      
                          
(defn sleeping->
  [person]
  (if (>= (:rest person) 100) :work
    (if (< (:rest person) 50) :sleep
      (if (> (predict-all person :sleep) (predict-all person :work))
        :sleep
        :work))))

(defn working->
  [person]
  (if (<= (:rest person) 0) :sleep
    (if (> (predict-all person :sleep) (predict-all person :work))
    	:sleep
      :work)))
      

(defn process-action
  [person] 
  (assoc person :action
    (case (:action person)
    	:sleep (if (>= (:rest person) 100) :work (sleeping-> person))
    	:work (if (<= (:rest person) 0) :sleep (working-> person)))))


(defn process-rest
  [person]
  (let [person-action (:action person)]
  	(let [rest-delta 
  		(case person-action
    		:work (- 0 work-fatigue-factor)
    		:sleep (current-rest-factor))]
    	(let [with-hist 
            (assoc person :history 
              (cons {:rest-delta rest-delta :night? (world/night?) :action person-action :rest (:rest person)} (:history person)))]
    		(assoc with-hist :rest (+ (:rest person) rest-delta))))))

(defn process-money
  [person]
  (let [money-delta 
		(if (= (:action person) :work)
      (current-productivity person)
      0)]
    (let [with-hist
    	(assoc person :history
        (cons (assoc (first (:history person)) :money-delta money-delta) (rest (:history person))))]
    	(assoc with-hist :money (+ (:money person) money-delta)))))

(defn process
  [person]
  (process-money (process-rest (process-action person))))


(defn process-all
  []
  (world/increment-hour)
  (def persons (into [] (for [p persons] (process p)))))

(defn process-and-print
  ([hours]
  	(doall (repeat hours (process-all)))
  	(println (world/hour-of-day))
  	(map #(println (str (:rest %) (:action %) (:money %))) persons)
   ;(clojure.pprint/pprint persons)
   )
  ([]
   (process-and-print 1)))