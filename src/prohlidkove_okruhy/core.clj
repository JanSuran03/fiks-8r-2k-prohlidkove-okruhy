(ns prohlidkove-okruhy.core
  (:require [clojure.string :as str])
  (:import (clojure.lang PersistentQueue)))

(def graph-data [[1 2] [2 5] [6 7]
                 [3 5] [4 8] [4 2]
                 [1 6] [6 8] [4 9]
                 [9 3] [8 9] [8 7]])

(defn directed-graph
  "Takes any number of edges (directly, not as a sequence of edges) where each of them
  is in a vector in format [vertex-1 vertex-2].

  The function returns a hash-map of the key as a literal and the value as a sequence of
  successors, e.g.:

  (directed-graph [v-1 v-2] [v-1 v-3] [v-2 v-5] [v-3 v-2] [v-3 v-4])
  => {v-1  (v-2 v-3)
      v-2  (v-5)
      v-3  (s-2 v-4)}

   Why this data structure? Well, after we pop a vertex from the queue during BFS, we
   want to quickly get its successors - by looking into this hash-map.

   We could have a hash-set of the successors since we don't care about their order, but
   for this task specifically, we don't need to - we're going to iterate over all of
   them anyway=> creating such a set would cost us some performance, we use a Clojure
   list (which works as a linked list) instead.

   It also makes inserting new vertices into an already existing list when a new edge
   from an already existing vertex is found super efficient - with O(1). If the vertex is
   not yet present in the adjacency map, we create a new linked list with the successor."
  [& edges]
  (loop [[[v1 v2 :as edge] & more] edges
         adjacency-map {}]
    (if edge
      (recur more
             (update adjacency-map v1 conj v2))
      adjacency-map)))

(defn breadth-first-search
  "Yeah. Breadth first search again. But... functional, immutable?

  Since we neither care about the shortest path nor about the route, we can use the good
  old breadth-first search which has the asymptotic computational complexity of O(V+E)
  where V is the count of vertices and E the count of edges.

  In this case, the BFS takes a directed graph as an adjacency map, the root and the
  target we want to find.

  First, we start our loop by initializing our collection of visited vertices as
  a hash-set with 1 element, the root, because we later want to quickly find out whether
  a vertex is already visited - by checking its presence in the hash-set, because that's
  exactly what a hash-set is used for.
  Then we initialize our queue with the root and do the following cycle. Remember we live
  in an immutable world, therefore we don't change the collections on place:
  LOOP 1:
     1) We pop the queue -> `popped-queue`.
     2) We mark the peek element as `peek-elem`.
     3) We get the `peek-elem`'s successors and mark them as `successors`.
     4) LOOP 2:
        `new-visited` = `visited`
        `new-queue` = `popped-queue`
        For every `successor` in `successors`:
        1) If the `successor` is nil (doesn't exist), return [`new-visited` `new-queue`].
        2) Else if the `successor` is the target vertex, return
           [`new-visited` `new-queue` true].
        3) Else if we already know the successor, go back to `1)` of `LOOP 2` with the
           next successor.
        4) Else (if the vertex is unknown), go back to `1)` of `LOOP 2` with the next
           successor and: 1) Add the successor to the set of `new-visited`.
                          2) Add the successor to `new-queue`.
     5) LOOP 2 returned: [`new-visited` `new-queue` *maybe-true*]:
        If *maybe-true* is true, the function returns true - the target has been found.
        If *maybe-true* is - in the case of 2nd step of `LOOP 2` - not present at all,
           (it is nil and therefore a logical false), The function returns to LOOP 1 with
           `new-visited` and `new-queue`.
  With this algorithm, we gradually get to the point where there won't be any other
  non-found successors and the queue will be empty. If we get to this point, the BFS
  algorithm will return false - our target was not found."
  [graph-as-adjacency-map root target]
  (loop [visited #{root}
         queue (conj PersistentQueue/EMPTY root)]
    (if (seq queue)
      (let [popped-queue (pop queue)

            peek-elem (peek queue)

            successors (get graph-as-adjacency-map peek-elem)

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