(ns
    #^{:author "Craig Ludington",
       :doc "Graphics for the Conway's Game of Life."}
  conway.graphics
  (require [clojure.contrib.math :as math])
  (use [conway.core :only (new-world)]
       [clojure.contrib.repl-utils :only (show apropos)]))

(defn make-frame
  [x y]
  (doto (java.awt.Frame.)
    (.setSize (java.awt.Dimension. x y))
    (.setTitle "Conway's Game of Life")
    (.setVisible true)))

(defn clear
  [gfx max-x max-y]
  (.clearRect gfx 0 0 max-x max-y))

(defn dot
  [gfx x y scale]
  (.fillRect gfx x y scale scale))

  (defn draw-world
    [gfx w scale]
    (doseq [[x y] w]
      (dot gfx (* x scale) (* y scale) scale)))

(defn run
  [world iterations size scale]
  (let [frame (make-frame size size)
	gfx (.getGraphics frame)]
    (loop [w world i 0]
	   (when (< i iterations)
	     (clear gfx size size)
	     (draw-world gfx w scale)
	     (Thread/sleep 250)
	     (recur (new-world w) (+ i 1))))
    (.dispose frame)))

(defn run-fast
  "Run without sleeping.  Return the final world and the frame in a vector."
  [world iterations size scale]
  (let [frame (make-frame size size)
	gfx (.getGraphics frame)]
    (loop [w world i 0]
      (if (< i iterations)
	(do (clear gfx size size)
	    (draw-world gfx w scale)
	    (recur (new-world w) (+ i 1)))
	[w frame]))))

(defn run-real-fast
  [world iterations size scale]
  (loop [w world i 0]
    (if (< i iterations)
      (recur (new-world w) (+ i 1))
      w)))

(def
 ^{:doc
   "The glider pattern has been proposed as the hacker emblem.
    It makes an interesting starting world.
    Try (run *glider* 100 200 5)
    http://www.catb.org/hacker-emblem/"
   }
 *glider*
     #{       [ 8 10]
                      [11  9]
       [9 10] [10 10] [11 10]
       })

;; Goran's collection
(def *f-pentomino* #{ [5,5],[5,6],[4,6],[6,6], [4,7] } )
(def *acorn* #{[3 5] [5 6] [3 6] [4 8] [5 9] [5 10] [5 11]})
(def *die-hard* #{[5 5] [5 6] [4 5] [4 6]  [4 10] [4 12] [6 11] [5 11]})
(def *lightweight-spaceship* #{[5 5] [5 6] [5 7] [5 8] [4 8] [3 8] [2 7] [4 4] [2 4]})
(def *blinker* #{[3 3] [3 4] [3 5]})

(defn shift-world
  "Move a world units in the x and y direction.  Helps get a map more centered on the visible board."
  [world units]
  (set (map (fn [[x y]] [(+ x units) (+ y units) ]) world)))

;; 327.4995 - This looks slow, and it is.
(defn bounding-box-a
  [world]
  {:pre [(seq world)
	 (every? vector? world)
	 (every? #(= 2 (count %)) world)
	 (every? (fn [[x y]] (and (integer? x)
				  (integer? y)))
		 world)]}
  (let [x #(% 0)
	y #(% 1)
	b (fn boundary [aggregator selector] (apply aggregator (map selector world)))]
    [[(b min x) (b min y)] [(b max x) (b max y)]]))

;; 133.3799 - Less than half the time of bounding-box-a
(defn bounding-box-b
  [world]
  {:pre [(seq world)
	 (every? vector? world)
	 (every? #(= 2 (count %)) world)
	 (every? (fn [[x y]] (and (integer? x)
				  (integer? y)))
		 world)]}
  (loop [w world
	 max-x 0
	 max-y 0
	 min-x 0
	 min-y 0]
    (if (seq w)
      (let [[x y] (first w)]
	(recur (rest w)
	       (max x max-x)
	       (max y max-y)
	       (min x min-x)
	       (min y min-y)))
      [[min-x min-y] [max-x max-y]])))

;; 204.4963  - Much slower than bounding-box-b ... type hints are the only difference
(defn bounding-box-c
  [world]
  {:pre [(seq world)
	 (every? vector? world)
	 (every? #(= 2 (count %)) world)
	 (every? (fn [[x y]] (and (integer? x)
				  (integer? y)))
		 world)]}
  (loop [w world
	 ^Integer max-x 0
	 ^Integer max-y 0
	 ^Integer min-x 0
	 ^Integer min-y 0]
    (if (seq w)
      (let [[^Integer x
	     ^Integer y] (first w)]
	(recur (rest w)
	       (max ^Integer x ^Integer max-x)
	       (max ^Integer y ^Integer max-y)
	       (min ^Integer x ^Integer min-x)
	       (min ^Integer y ^Integer min-y)))
      [[min-x min-y] [max-x max-y]])))

(defn bounding-box-d
  [world]
  {:pre [(seq world)
	 (every? vector? world)
	 (every? #(= 2 (count %)) world)
	 (every? (fn [[x y]] (and (integer? x)
				  (integer? y)))
		 world)]}
  (loop [w world
	 max-x (int 0)
	 max-y (int 0)
	 min-x (int 0)
	 min-y (int 0)]
    (if (seq w)
      (let [[x y] (first w)]
	(let [x (int x) y (int y)]
	  (recur (rest w)
		 (int (max x max-x)) 
		 (int (max y max-y))
		 (int (min x min-x))
		 (int (min y min-y)))))
      [[min-x min-y] [max-x max-y]])))

(defn random-coord [max] [(math/round (rand max)) (math/round (rand max))])
(defn random-world [size max] (set (take size (repeatedly (partial random-coord max)))))