(ns
    #^{:author "Craig Ludington",
       :doc "Graphics for the Conway's Game of Life."}
  conway.graphics
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
(def *acron* #{[3 5] [5 6] [3 6] [4 8] [5 9] [5 10] [5 11]})
(def *die-hard* #{[5 5] [5 6] [4 5] [4 6]  [4 10] [4 12] [6 11] [5 11]})
(def *lightweight-spaceship* #{[5 5] [5 6] [5 7] [5 8] [4 8] [3 8] [2 7] [4 4] [2 4]})
(def *blinker* #{[3 3] [3 4] [3 5]})

(defn shift-world
  "Move a world units in the x and y direction.  Helps get a map more centered on the visible board."
  [world units]
  (set (map (fn [[x y]] [(+ x units) (+ y units) ]) world)))


(let [x (fn x [a] (a 0))
      y (fn y [a] (a 1))
      b (fn boundary [world f selector] (apply f (map selector world)))]
  (defn max-x [w] (b w max x))
  (defn max-y [w] (b w max y))
  (defn min-x [w] (b w min x))
  (defn min-y [w] (b w min y))
  (defn bounding-rectangle [w] [[(min-x w) (min-y w)] [(max-x w) (max-y w)]]))

