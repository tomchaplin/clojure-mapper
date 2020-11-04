(ns mapper.cluster-test
  (:require [clojure.test :refer :all]
            [mapper.cluster :as clust]
            [clojure.core.matrix :as mat]))

(def test-matrix
  (mat/matrix [[0 17 21 31 23]
               [17 0 30 34 21]
               [21 30 0 28 39]
               [31 34 28 0 43]
               [23 21 39 43 0]]))

(def test-d
  (clust/d-from-mat test-matrix))

(def test-sl
  (clust/single-linkage test-d (mat/column-count test-matrix)))

(def test-linkages
  (:linkages test-sl))

(def test-merges
  (:merges test-sl))

(deftest merging-test
  (testing "Resolving merges into clusters"
    (is 
      (=
        (clust/merges-to-clusters (mat/column-count test-matrix) (subvec test-merges 0 2))
        [#{0 1 2} #{3} #{4}]))
   (is 
      (=
        (clust/merges-to-clusters (mat/column-count test-matrix) (subvec test-merges 0 3))
        [#{0 1 2 4} #{3}]))))

(deftest linkage-test
  (testing "Single linkage algorithm with 5x5"
    (is (= test-linkages [17 21 21 28]))
    (is (= test-merges [[0 1] [0 1] [0 2] [0 1]]))))
