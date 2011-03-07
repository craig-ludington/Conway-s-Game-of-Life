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

(comment
  (defn dot
    [gfx x y]
    (.fillRect gfx x y 1 1))

  (defn draw-world
    [gfx w]
    (doseq [[x y] w]
      (dot gfx x y)))
  )


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
	(Thread/sleep 1000)
	(recur (new-world w) (+ i 1))))
    (.dispose frame)))

;; 
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