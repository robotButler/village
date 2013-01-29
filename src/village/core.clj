(ns village.core)

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

(defn -main [] (foo "bill"))

(defn prompt
  "WAit for user command"
  []
  (println "?>")
  (read))