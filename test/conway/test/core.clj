(ns conway.test.core
  (:use [conway.core] :reload)
  (:use [clojure.test]))

(deftest neighborhood-works
  (is (= (neighborhood [0 0]) *offsets*)))

;;         Y
;;
;;        -2 +------+------+------+------+
;;           |      |      |      |      |
;;           |      |      |      |      |
;;        -1 +------+------+------+------+
;;           |      |      |      |      |
;;           |      |      |      |      |
;;         0 +------C------C------C------+
;;           |      |      |      |      |
;;           |      |      |      |      |
;;         1 +------+------+------+------+
;;           |      |      |      |      |
;;           |      |      |      |      |
;;         2 +------+------+------+------+
;;          -2     -1      0      1      2   X
;;
;;  Starting World  #{ [-1 0] [0 0] [1 0] }
(deftest neighbors-works
  (let [world #{[-1  0]
		[ 0  0]
		[ 1  0] }]
    (is (= (neighbors world [0 0])
	   [[-1 0]
	    [ 1 0]]))
    (is (not (seq (neighbors world [2 2]))))))

;;         Y
;;
;;        -2 +------+------+------+------+
;;           |      |      |      |      |
;;           |      |      |      |      |
;;        -1 +------+------P------+------+
;;           |      |      |      |      |
;;           |      |      |      |      |
;;         0 +------C------C------C------+
;;           |      |      |      |      |
;;           |      |      |      |      |
;;         1 +------+------P------+------+
;;           |      |      |      |      |
;;           |      |      |      |      |
;;         2 +------+------+------+------+
;;          -2     -1      0      1      2   X
;;
;; World Pregnancies #{ [0 -1] [0 1] }
(deftest pregant?-works
  (let [world #{[-1  0]
		[ 0  0]
		[ 1  0] }]
    (is (not (pregnant? world [ 0  0]))) ;; There's a live cell there
    (is (not (pregnant? world [-1 -1]))) ;; Only 2 neighbors
    (is (not (pregnant? world [-2  0]))) ;; Only 1 neighbor
    (is (pregnant? world [0  1]))        ;; 3 neighbors
    (is (pregnant? world [0 -1]))))      ;; 3 neighbors

(deftest spawn-works
  (let [world #{[-1  0]
		[ 0  0]
		[ 1  0] }
	births #{[0 -1]
		 [0 1]}]
    (is (= births (spawn world)))))

(deftest survivors-works
  (let [world #{[-1  0]
		[ 0  0]
		[ 1  0]}
	keep  #{[0 0]}]
    (is (= keep (survivors world)))))

;;         Y
;;
;;        -2 +------+------+------+------+
;;           |      |      |      |      |
;;           |      |      |      |      |
;;        -1 +------+------C------+------+
;;           |      |      |      |      |
;;           |      |      |      |      |
;;         0 +------+------C------+------+
;;           |      |      |      |      |
;;           |      |      |      |      |
;;         1 +------+------C------+------+
;;           |      |      |      |      |
;;           |      |      |      |      |
;;         2 +------+------+------+------+
;;          -2     -1      0      1      2   X
;;
;;  New World  #{ [-1 0] [0 0] [1 0] }
(deftest new-world-works
  (let [world #{[-1  0]
		[ 0  0]
		[ 1  0]}
	new   #{[ 0 -1]
		[ 0  0]
		[ 0  1]}]
    (is (= new (new-world  world)))))