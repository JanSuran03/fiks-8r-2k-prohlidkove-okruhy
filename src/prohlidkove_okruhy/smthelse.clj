(ns prohlidkove-okruhy.smthelse
  (:require [clojure.string :as str])
  (:import (clojure.lang ITransientMap PersistentQueue)))

(def graph-data [[1 2] [2 5] [6 7]
                 [3 5] [4 8] [4 2]
                 [1 6] [6 8] [4 9]
                 [9 3] [8 9] [8 7]])

(defn- update!
  "An `update` function for a transient map, a faster version than the regular one. "
  [^ITransientMap m k f x]
  (assoc! m k (f (get m k) x)))

(defn directed-graph [& edges]
  (loop [vertices (transient #{})
         [[v1 v2 :as edge] & more] edges
         adjacency-map (transient {})]
    (if edge
      (recur (-> vertices (conj! v1) (conj! v2))
             more
             (update! adjacency-map v1 (fnil conj #{}) v2))
      {:adjacency-map (persistent! adjacency-map)
       :vertices      (persistent! vertices)})))

(defn breadth-first-search [{:keys [adjacency-map vertices] :as graph} root target]
  (loop [visited #{root}
         queue (conj PersistentQueue/EMPTY root)]
    (if (seq queue)
      (let [popped-queue (pop queue)

            first-in-queue (peek queue)

            successors (get adjacency-map first-in-queue)

            [new-visited new-queue found?]
            (loop [new-visited visited
                   new-queue popped-queue
                   [first-successor & remaining-successors] successors]
              (cond (not first-successor)
                    [new-visited new-queue]

                    (= first-successor target)
                    [new-visited new-queue true]

                    (contains? visited first-successor)
                    (recur new-visited
                           new-queue
                           remaining-successors)

                    :else
                    (recur (conj new-visited first-successor)
                           (conj new-queue first-successor)
                           remaining-successors)))]
        (if found?
          true
          (recur new-visited
                 new-queue)))
      false)))