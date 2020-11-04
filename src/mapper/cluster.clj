(ns mapper.cluster
  (:require
    [clojure.core.matrix :as mat]
    [clojure.set :as clset]))

(defn d-from-mat [m]
  (fn [x y]
    (mat/select m x y)))

(defn closest-clusters
  [d n]
  (loop [i 0 j 1 min-val ##Inf min-index [0 1]]
    (if (= j n)
      (if (= i (- n 2))
        ; We are done so return
        {:min-index min-index :min-val min-val}
        ; We must go to the next row start off-diagonal
        (recur (+ i 1) (+ i 2) min-val min-index)
        )
      ; We need to check this value and move along a column
      (let [new-value (d i j)]
        (if (< new-value min-val)
          (recur i (+ j 1) new-value [i j])
          (recur i (+ j 1) min-val min-index))))))

(defn translate-non-zero-index
  [i [ left-idx right-idx]]
  (if (<= i left-idx)
    ; The cluster has been added before us so we get shifted
    (- left-idx 1)
    (if (< i right-idx)
      ; Cluster added but one column remove so no change
      i
      ; Total of one column removed to our left so we go other direction
      (+ i 1))))

(defn compute-new-d
  [d [left-idx right-idx]]
  ; We compute a new distance func
  ; Assuming left-idx < right-idx
  ; The new cluster gets put in index 0
  (fn [a b]
    (let [x (min a b)
          y (max a b)]
      (if (= x 0)
        (let [old-y (translate-non-zero-index y [left-idx right-idx])]
          (min (d left-idx old-y) (d right-idx old-y)))
        (let [old-x (translate-non-zero-index x [left-idx right-idx])
              old-y (translate-non-zero-index y [left-idx right-idx])]
          (d old-x old-y))))))

(defn merges-to-clusters
  "Takes a merge-list and a number of nodes and outputs clusters from those merges.
   This assumes that after each merge the merged cluster is pushed to index 0 and everything
   else reshuffles
   Input:
   n - A number of nodes
   merges - [[0 1][1 2][0 1] ...]

   Output:
   [#{0 1 3} #{2 4} #{5} ...]"
  [n merges]
  (loop [remaining-merges   merges
         clusters            (mapv #(#{%}) (range n))]
    (if (empty? remaining-merges)
      clusters
      (let [[key-a key-b]     (first remaining-merges)
            {cluster-a key-a
             cluster-b key-b} clusters
            new-cluster       (clset/union cluster-a cluster-b)
            remove-merged     (keep-indexed 
                                (fn [index val]
                                  (if (not (or (= index key-a) (= index key-b)))
                                    val
                                    nil))
                                clusters)]
        ; TODO: Prepending new cluster to vector is O(n) slow, should re-do
        (recur (rest remaining-merges) (into [new-cluster] remove-merged))))))

(defn single-linkage
  "Performs a single linkage clustering.

  Input:
  d - A function accepting integers x, y and computing distance between node x and node y (0-index)
  n - The number of nodes (>= 2)
  
  Output:
  {:linkages [...] :merges [[k1 k2] [k3 k4]...]}
  Where linkages are the lengths of the linkages added and merges is a vector
  of cluster indices that get merged at each stage. Note when two clusters are merged
  the resulting cluster is pushed to the start"
  ; TODO: Should we push it to the end for better efficieny and index naming sanity!?
  ([d n linkages merges]
  ; Init linkages to [] if not defined
    (if (= n 2)
      ; There are two clusters remaining so only one more linkage needed
      { :linkages (conj linkages (d 0 1)) :merges (conj merges [0 1])} 
      ; We need to figure out which clusters to link then remove them
      (let [closest           (closest-clusters d n)
            link-length       (:min-val closest)
            link-pair         (:min-index closest)
            new-d             (memoize (compute-new-d d link-pair))]
        ; Add the new link to the linkages array 
        ; And call the clustering algorthm on the remaining matrix
        (single-linkage new-d (- n 1) (conj linkages link-length) (conj merges link-pair)))))
  ([d n] 
    (single-linkage (memoize d) n [] [])))

(defn single-linkage-cluster
  "Perform single-linkage merging then histogram the linkages into k equal intervals.
  Select an appropriate number of merges to do and return clustering"
  [d n k]
  nil)
