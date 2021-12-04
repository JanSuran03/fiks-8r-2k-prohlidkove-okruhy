(ns prohlidkove-okruhy.core
  (:require [clojure.string :as str]
            [loom.graph :refer :all]
            [loom.alg :as alg]))

(defn read-and-process-input []
  (let [[[_num-crossroads num-edges _num-queries] & input] (->> "input.txt" slurp
                                                                str/split-lines
                                                                (map #(as-> % <>
                                                                            (str/split <> #"\ ")
                                                                            (map read-string <>))))]
    (split-at num-edges input)))

(defn path-exists? [graph [vertex-1 vertex-2 :as _query]]
  (if (alg/bf-path graph vertex-1 vertex-2)
    "Cesta existuje"
    "Cesta neexistuje"))

(defn -main [& args]
  (let [[edges queries] (read-and-process-input)
        graph (apply digraph edges)]
    (->> queries (map #(path-exists? graph %))
         (str/join "\n")
         (spit "output.txt"))))