(ns mapper.core
  (:require [clojure.set :as clset]))

(defn compute-neighbours
  [node node-index nodes]
  (keep-indexed 
    ; When this node intersects non-empty with another node then keep the index of that other node
    ; Provided it is not this node (preventing self-loops)
    #(when 
      (and (not-empty (clset/intersection node %2))
           (not= %1 node-index))
      %1)
    nodes))

(defn set-from-indicator
  "Given an indicator and a domain returns the set of a points on that domain where the indicator is true"
  [indicator domain]
  (set (for [x domain :when (indicator x)] x)))

(defn create-mapper
  "Input:
  f - The filter function
  get-pullback-indicators - A function accepting f and a set of points and returning a seq of indicator functions for the pull-back cover
  clustering - A function accepting a set A and returning a seq of sets describing a clustering of A
  coloring - A function accepting a set of points and returning an annotation for that set of points in the output graph

  Output:
  mapper - A mapper algorithm which accept a set of points
  
  Note:
  The points can be whatever you want them to be"
  [f get-pullback-indicators clustering coloring]
  (fn [points]
    (let [pullback-indicators   (get-pullback-indicators f points)
          pullback-cover        (map #(set-from-indicator % points) pullback-indicators)
          nodes                 (apply concat (map clustering pullback-cover))]
      (map-indexed
        (fn [node node-index]
          {:set node
           :neighbours (compute-neighbours node node-index nodes)
           :color (coloring node)
           })
        nodes))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
