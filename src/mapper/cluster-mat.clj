(ns mapper.cluster-mat
  (:require
    [clojure.core.matrix :as mat]))


(defn ereduce-indexed
  [f init matrix]
  ( :working (mat/ereduce 
    ; The function passes through the working object
    ; Also provides next-elem of matrix
    ; But also the index
    ; The wrapper then replaces the working object and removes the first index
    (fn [higher-working next-elem]
      { :working (f (:working higher-working) next-elem (first (:indices higher-working)) )
        :indices (rest (:indices higher-working) )}) 
    {:working init :indices (mat/index-seq matrix)} 
    matrix)))

; TODO: Implement
(defn closest-clusters
  "Input:
  dist_matrix
  
  Output:
  A map { :pair [left-idx right-idx] 
          :length link-length} for the two clusters that are closest"
  [dist-matrix]
  (ereduce-indexed (fn [working next-elem index]
                    (if (< next-elem (:length working))
                      ; If on diagonal then ignore
                      (if (= (get index 0) (get index 1)) 
                        working
                        {:length next-elem
                         :pair index})
                      working))
                   {:length ##Inf
                    :pair []}
                   dist-matrix
  ))

; TODO: Implement
(defn without
  "Input:
  dist_matrix left-idx right-idx
  
  Output:
  A new risk matrix with the two clusters merged"
  [dist-matrix left-idx right-idx]
  dist-matrix)

(defn single-linkage
  "Performs a single linkage clustering.

  Input:
  dist-matrix - a distance matrix for the nodes
  
  Output:
  A vector of the linkages required to merge the nodes to a single cluster"
  [dist-matrix & [linkages]]
  ; Init linkages to [] if not defined
  (let [linkages (or linkages [])] 
    (if ( < (mat/column-count dist-matrix) 2)
      ; There are two clusters remaining so only one more linkage needed
      (conj linkages (mat/select dist-matrix 1 1)) 
      ; We need to figure out which clusters to link then remove them
      (let [[left-idx right-idx link-length]  (closest-clusters dist-matrix)
            new-matrix            (without dist-matrix left-idx right-idx)]
        ; Add the new link to the linkages array 
        ; And call the clustering algorthm on the remaining matrix
        (single-linkage new-matrix (conj linkages link-length))
        ))))

