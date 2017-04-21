;; Grand Theft Wumpus
;; ==================
;;
;; Land of Lisp
;; ============
;;
;; Author:
;; ======
;;  Barski, C. "Land of Lisp: Learn to Program in Lisp, One Game at a Time!",
;; 2011.
;;
;; Copyright © 2011 by Conrad Barski, M.D.

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

(defun make-city-edges ()
  ;; Function
  ;; ========
  ;;
  ;; Output:
  ;; ======
  ;;  Create the edges of Congetion-City with cops
  (let* ((nodes (loop for i from 1 to *node-num*
		      collect i))
	 ;; Local-parameter
	 ;; ===============
	 ;;
	 ;; Description:
	 ;; ===========
	 ;;  List of nodes
	 (edge-list (connect-all-islands nodes (make-edge-list)))
	 ;; Local-parameter
	 ;; ===============
	 ;;
	 ;; Description:
	 ;; ===========
	 ;;  Creates a random (but fully connected) edge list
	 (cops
	  ;; Local-parameter
	  ;; ===============
	  ;;
	  ;; Description
	  ;; ===========
	  ;;  Creates a random list of edges that contains cops
	  (remove-if-not (lambda (x)
			   ;; Lambda-function
			   ;; ===============
			   ;;
			   ;; Input:
			   ;; =====
			   ;;  [1] - x, a edge
			   ;;
			   ;; Output:
			   ;; ======
			   ;;  Choose if x can have a cop-odd or not.
			   (zerop (random *cop-odds*)))
			 edge-list)))
    (add-cops (edges-to-alist edge-list) cops)))

(defun edges-to-alist (edge-list)
  ;; Function
  ;; ========
  ;;
  ;; Input:
  ;; =====
  ;;  [1] - edge-list, a list of edges of Congetion-City
  ;;
  ;; Output:
  ;; ======
  ;;  Converts a list of edges into an alist of edges
  ;;
  ;; Example:
  ;; =======
  ;;  [1] > (edges-to-alist '((1 . 2) (2 . 1) (2 . 3) (3 . 2)))
  ;;      > '((1 (2)) (2 (1) (3)) (3 (2)))
  (mapcar (lambda (node1)
	    ;; Lambda-function
	    ;; ===============
	    ;;
	    ;; Input:
	    ;; =====
	    ;;  [1] - node1, A node to remove duplicates
	    ;;
	    ;; Output:
	    ;; ======
	    ;;  Nested mapcar functions allow edges-to-alist to convert the edges
	    ;; of a city into an alist.
            (cons node1
                  (mapcar (lambda (edge)
			    ;; Lambda-function
			    ;; ===============
			    ;;
			    ;; Input:
			    ;; =====
			    ;;  [1] - edge,
			    ;;
			    ;; Output:
			    ;; ======
			    ;;  To build the list of nodes, we use the
			    ;; remove-duplicates function, which removes
			    ;; duplicate items from a list.
			    (list (cdr edge)))
			  (remove-duplicates (direct-edges node1 edge-list)
					     :test #'equal))))
	  (remove-duplicates (mapcar #'car edge-list))))

(defun add-cops (edge-alist edges-with-cops)
  ;; Function
  ;; ========
  ;;
  ;; Input:
  ;; =====
  ;;  [1] - edge-alist, all edges of the city
  ;;  [2] - edge-with-cops, the edges marked with cops-odds
  ;;
  ;; Output:
  ;; ======
  ;;  Use this list of cop edges to mark the edges in our alist that contain cops
  (mapcar (lambda (x)
	    ;; Lambda-function
	    ;; ===============
	    ;;
	    ;; Input:
	    ;; =====
	    ;;  [1] - x, a simple edge with format (a (b) (c))
	    ;;
	    ;; Output:
	    ;; ======
	    ;;  Nested mapcar commands to map across the edges within each node
	    (let ((node1
		   ;; Local-parameter
		   ;; ===============
		   ;;
		   ;; Description:
		   ;; ===========
		   ;;  Knowing that x have the format (a (b) (c)), node1 is a
		   (car x))
		  (node1-edges
		   ;; Local-parameter
		   ;; ===============
		   ;;
		   ;; Description:
		   ;; ===========
		   ;;  Knowing that x have the format (a (b) (c)), node1 is
		   ;; ((b) (c))
		   (cdr x)))
	      (cons node1
		    (mapcar (lambda (edge)
			      ;; Lambda-function
			      ;; ===============
			      ;;
			      ;; Input:
			      ;; =====
			      ;;  [1] - edge, the edges of the city
			      ;;
			      ;; Output:
			      ;; ======
			      ;;  We then check whether there are any cops on a
			      ;; given edge, using the intersection function.
			      (let ((node2
				     ;; Local-parameter
				     ;; ===============
				     ;;
				     ;; Description:
				     ;; ===========
				     ;;  The first node of edge
				     (car edge)))
				(if (intersection (edge-pair node1 node2)
						  edges-with-cops
						  :test #'equal)
				    (list node2 'cops)
				  edge)))
			    node1-edges))))
	  edge-alist))

(defun neighbors (node edge-alist)
  ;; Function
  ;; ========
  ;;
  ;; Input:
  ;; =====
  ;;  [1] - node, a simple node
  ;;  [2] - edge-alist, alist of edges
  ;;
  ;; Output:
  ;; ======
  ;;  Looks up the node’s neighbors using the alist of edges.
  ;; If the second node is in that list, we know we’re one away.
  (mapcar #'car (cdr (assoc node edge-alist))))


(defun within-one (a b edge-alist)
  ;; Function
  ;; ========
  ;;
  ;; Input:
  ;; =====
  ;;  [1] - a,
  ;;  [2] - b,
  ;;  [3] - edge-alist,
  ;;
  ;; Output:
  ;; ======
  ;;  Looks up the first node (a) in the alist of edges with
  ;; neighbors. Then it uses member to see if the other node
  ;; (b) is among these nodes.
  (member b (neighbors a edge-alist)))

(defun within-two (a b edge-alist)
  ;; Function
  ;; ========
  ;;
  ;; Input:
  ;; =====
  ;;  [1] - a,
  ;;  [2] - b,
  ;;  [3] - edge-alist,
  ;;
  ;; Output:
  ;; ======
  ;;  
  (or (within-one a b edge-alist)
      (some (lambda (x)
	      ;; Lambda-function
	      ;; ===============
	      ;;
	      ;; Input:
	      ;; =====
	      ;;  [1] - x,
	      ;;
	      ;; Output:
	      ;; ======
	      ;;  
              (within-one x b edge-alist))
            (neighbors a edge-alist))))
