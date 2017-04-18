;; Libraries
;; =========

(load "graph-util")

;; Global Parameters
;; =================

(defparameter *congestion-city-nodes* nil)

(defparameter *congestion-city-edges* nil)

(defparameter *visited-nodes* nil)

(defparameter *node-num*
  ;; Global-parameter
  ;; ================
  ;;
  ;; Description
  ;; ===========
  ;;  Defines the number of location of Congestion City
  30)

(defparameter *edge-num*
  ;; Global-parameter
  ;; ================
  ;;
  ;; Description:
  ;; ===========
  ;;  Defines the number of roads between locations
  45)

(defparameter *worm-num*
  ;; Global-parameter
  ;; ================
  ;;
  ;; Description:
  ;; ===========
  ;;  The number of Gruesome Glowworm Gang teams
  3)

(defparameter *cop-odds*
  ;; Global-parameter
  ;; ================
  ;;
  ;; Description:
  ;; ===========
  ;;  The number of cops units in Congestion City
  15)

(defun random-node ()
  ;; Function
  ;; ========
  ;;
  ;; Output:
  ;; ======
  ;;  Returns a random node identifier
  (1+ (random *node-num*)))

(defun edge-pair (a b)
  ;; Function
  ;; ========
  ;;
  ;; Input:
  ;; =====
  ;;  [1] - a, First Node
  ;;  [2] - b, Second Node
  ;;
  ;; Output:
  ;; ======
  ;;  Create two directed edges between the randomly
  ;; selected nodes. This step makes sense only for
  ;; undirected graph, with two opposing directed edges
  ;; mirroring each undirected edge.
  (unless (eql a b)
    (list (cons a b)
	  (cons b a))))

(defun make-edge-list ()
  ;; Function
  ;; ========
  ;;
  ;; Output:
  ;; ======
  ;;  Generates the actual list of random edges.
  (apply #'append (loop repeat *edge-num*
			collect (edge-pair (random-node)
					   (random-node)))))


(defun direct-edges (node edge-list)
  ;; Function
  ;; ========
  ;;
  ;; Input:
  ;; =====
  ;;  [1] - node, a node
  ;;  [2] - edge-list, a list of edges
  ;;
  ;; Output:
  ;; ======
  ;;  Finds the edges connect if the nodes.
  (remove-if-not (lambda (x)
		   ;; Lambda-function
		   ;; ===============
		   ;;
		   ;; Input:
		   ;; =====
		   ;;  [1] - x, a edge of edge-list
		   ;;
		   ;; Output:
		   ;; ======
		   ;;  Apply remove-if-not on the head of each
		   ;; alist element of edge-list that is equal
		   ;; of node.
		   (eql (car x) node))
		 edge-list))

(defun get-connected (node edge-list)
  ;; Function
  ;; ========
  ;;
  ;; Input:
  ;; =====
  ;;  [1] - node, a source node
  ;;  [2] - edge-list, the list of nodes
  ;;
  ;; Output:
  ;; ======
  ;;  Builds a list of all nodes connected to that node, even if
  ;; it requires walking across multiple edges.
  (let ((visited nil))
    ;; Local-parameter
    ;; ===============
    ;;
    ;; Description:
    ;; ===========
    ;;  A visited node list
    (labels ((traverse (node)
		       ;; Local-Function
		       ;; ==============
		       ;;
		       ;; Input:
		       ;; =====
		       ;;  [1] - node, a node to travel
		       ;;
		       ;; Output:
		       ;; ======
		       ;;  Seach along connected nodes, starting
		       ;; with the source node. Newly found
		       ;; nodes are added to the visited list.
		       (unless (member node visited)
			 (push node visited)
			 (mapc (lambda (edge)
				 ;; Lambda-Function
				 ;; ===============
				 ;;
				 ;; Input:
				 ;; =====
				 ;;  [1] - edge, a edge of node.
				 ;;
				 ;; Output:
				 ;; ======
				 ;;  traverse all the children
				 ;; of this found node.
				 (traverse (cdr edge)))
			       (direct-edges node edge-list)))))
	    (traverse node))
    visited))

(defun find-islands (nodes edge-list)
  ;; Function
  ;; ========
  ;;
  ;; Input:
  ;; =====
  ;;  [1] - nodes, a list of nodes
  ;;  [2] - edge-list, a list of edges
  ;;
  ;; Output:
  ;; ======
  ;;  Returns a list of nodes that are not connected (it is a
  ;; island).
  (let ((islands nil))
    ;; Local-parameter
    ;; ===============
    ;;
    ;; Description:
    ;; ===========
    ;;  A list of nodes that are not in edge-list.
    (labels ((find-island (nodes)
			  ;; Local-fucntion
			  ;; ==============
			  ;;
			  ;; Input:
			  ;; =====
			  ;;  [1] - nodes, a list of nodes
			  ;;
			  ;; Output:
			  ;; ======
			  ;;  Checks which nodes are connected
			  ;; to the first node in our list of
			  ;; nodes using the connected function.
			  ;; It then subtracts these nodes from
			  ;; the full list of nodes using the
			  ;; set-difference function.
			  (let* ((connected (get-connected (car nodes) edge-list))
				 ;; Local-local-parameter
				 ;; =====================
				 ;;
				 ;; Description:
				 ;; ===========
				 ;;  A list of connected node.
				 (unconnected (set-difference nodes connected))
				 ;; Local-local-parameter
				 ;; =====================
				 ;;
				 ;; Description:
				 ;; ===========
				 ;;  A list of desconnected node
				 )
			    (push connected islands)
			    (when unconnected
			      (find-island unconnected)))))
	    (find-island nodes))
    islands))

(defun connect-with-bridges (islands)
  ;; Function
  ;; ========
  ;;
  ;; Input:
  ;; =====
  ;;  [1] - islands, a list of not connected node
  ;;
  ;; Output:
  ;; ======
  ;;  Returns a list of additional edges that join all the
  ;; islands together.
  (when (cdr islands)
    (append (edge-pair (caar islands)
		       (caadr islands))
	    (connect-with-bridges (cdr islands)))))

(defun connect-all-islands (nodes edge-list)
  ;; Function
  ;; ========
  ;;
  ;; Input:
  ;; =====
  ;;  [1] - nodes, all nodes
  ;;  [2] - edge-list, the complete edge-list
  ;;
  ;; Output:
  ;; ======
  ;;  Tie all of our island prevention functions together
  (append (connect-with-bridges (find-islands nodes edge-list)) edge-list))
