(ns prohlidkove-okruhy.core
  (:require [clojure.string :as str]
            [loom.graph :refer :all]
            [loom.alg :as alg]))

(defn read-and-process-input []
  (->> "input.txt" slurp
       str/split-lines))

(defn -main [& args])
