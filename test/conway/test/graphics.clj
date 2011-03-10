(ns conway.test.graphics
  (:use [conway.graphics] :reload)
  (:use [clojure.test]))

(deftest bounding-box-works
  (is (= (bounding-box #{ [0 0] })
	 [[0 0] [0 0]]))
  (is (= (bounding-box #{ [0 0] [1 1] })
	 [[0 0] [1 1]]))
  (is (= (bounding-box #{ [0 0] [1 1] [0 3] })
	 [[0 0] [1 3]])))
