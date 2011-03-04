(ns
    #^{:author "Craig Ludington",
       :doc "Implement Conway's Game of Life functionally in an infinite world.
 Rules:
   Any live cell with fewer than two live neighbours dies, as if caused by under-population.
   Any live cell with two or three live neighbours lives on to the next generation.
   Any live cell with more than three live neighbours dies, as if by overcrowding.
   Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction.

 How to use it:
   Define a world as a set of coordinates, where each coordinate is a vector of two integers.
   Pass that world as a parameter to new-world.
   A new world will be returned.
   Repeat forever.

For example:
   (new-world #{ [-1 0] [0 0] [1 0] })
   => #{[0 -1] [0 0] [0 1]}

"}
  conway.core
  (use [clojure.contrib.combinatorics :only (cartesian-product)]))

(def
  ^{:doc "Offsets for neighbors of a coordinate."}
  *offsets*
  (remove #{[0 0]}
	  (map (partial into [])
	       (cartesian-product [-1 0 1] [-1 0 1]))))

(defn neighborhood
  "A vector of neighboring coordinates for a coordinate."
  [[x y]]
  (map (fn vector-add [[xx yy]] (vector (+ x xx) (+ y yy)))
       *offsets*))

(defn neighbors
  "A vector of (live) cells neighboring a coordinate."
  [world coordinate]
  (filter world (neighborhood coordinate)))

(defn pregnant?
  "True if a coordinate has no live cell and has exactly three neighbors."
  [world coordinate]
  (when (and (not (world coordinate))
	     (= 3 (count (neighbors world coordinate))))
    coordinate))

(defn spawn
  "Return all the new live cells in the world."
  [world]
  (set (filter (partial pregnant? world)
	       (reduce into [] (map neighborhood world)))))

(defn healthy?
  "Return cell if it is healthy enough to survive."
  [world cell]
  (when (and (world cell)
	     (#{2 3} (count (neighbors world cell)))) 
    cell))

(defn survivors
  "Return all the living cells that survive into the next world."
  [world]
  (set (filter (partial healthy? world) world)))

(defn new-world
  "Return a new world."
  [world]
  (into (survivors world) (spawn world)))
