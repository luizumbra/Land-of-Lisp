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

;; Define the Edges
;; ================

(load "graph-util")


(defparameter *congestion-city-nodes*
  ;; Global-Parameter
  ;; ================
  ;;
  ;; Description:
  ;; ===========
  ;;  A set of nodes stored. The possible data at each node will include the presence
  ;; of the Wumpus, a Glowworm team, and various danger signs.
  nil)


(defparameter *congestion-city-edges*
  ;; Global-Parameter
  ;; ================
  ;;
  ;; Description:
  ;; ===========
  ;;  A set of edges stored and data linked to these edges will alert us to the
  ;; presence of any police roadblocks.
  nil)


(defparameter *visited-nodes* nil)


(defparameter *node-num*
  ;; Global-Parameter
  ;; ================
  ;;
  ;; Description:
  ;; ===========
  ;;  Maximum number of nodes.
  30)


(defparameter *edge-num*
  ;; Global-Parameter
  ;; ================
  ;;
  ;; Description:
  ;; ===========
  ;;  Maximum number of edges.
  45)


(defparameter *worm-num*
  ;; Global-Parameter
  ;; ================
  ;;
  ;; Description:
  ;; ===========
  ;;  Maximum number of worms team.
  3)


(defparameter *cop-odds*
  ;; Global-Parameter
  ;; ================
  ;;
  ;; Description:
  ;; ===========
  ;;  Each street will have a 1-in-15 chance of containing a roadblock.
  15)

;; Generating Random Edges
;; =======================

(defun random-node ()
  ;; Function
  ;; ========
  ;;
  ;; Output:
  ;; ======
  ;;  Returns a random node identifier.
  (1+ (random *node-num*)))

(defun edge-pair (a b)
  ;; Function
  ;; ========
  ;;
  ;; Input:
  ;; =====
  ;;  [1] - a, a node.
  ;;  [2] - b, a node.
  ;;
  ;; Output:
  ;; ======
  ;;  It is a helper function to create two direction edges between 'a' and 'b'.
  ;;
  ;; Run:
  ;; ===
  ;;  [1] > (edge-pair 1 2)
  ;;        ((1 . 2) (2 . 1))
  ;;
  ;;  [2] > (edge-pair "a" "b")
  ;;        (("a" . "b") ("b" . "a"))
  ;;
  ;;  [3] > (edge-pair '(1 2 3) '(3 2))
  ;;        (((1 2 3) 3 2) ((3 2) 1 2 3))
  ;;
  ;;  [4] > (edge-pair '(1 2 3) 3)
  ;;        (((1 2 3) . 3) (3 1 2 3))
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
  ;;
  ;; Run:
  ;; ===
  ;;  [1] - *edge-num* with 45 value, and remember, it is use a random function (random-node)
  ;;      > (make-edge-list)
  ;;        ((18 . 1) (1 . 18) (4 . 6) (6 . 4) (16 . 9) (9 . 16) (12 . 26) (26 . 12)
  ;;         (10 . 30) (30 . 10) (9 . 13) (13 . 9) (7 . 8) (8 . 7) (3 . 7) (7 . 3) (6 . 2)
  ;;         (2 . 6) (10 . 26) (26 . 10) (23 . 10) (10 . 23) (10 . 12) (12 . 10) (18 . 23)
  ;;         (23 . 18) (2 . 22) (22 . 2) (23 . 18) (18 . 23) (17 . 6) (6 . 17) (13 . 12)
  ;;         (12 . 13) (30 . 13) (13 . 30) (22 . 25) (25 . 22) (26 . 16) (16 . 26)
  ;;         (30 . 18) (18 . 30) (30 . 29) (29 . 30) (28 . 25) (25 . 28) (10 . 2) (2 . 10)
  ;;         (5 . 18) (18 . 5) (30 . 19) (19 . 30) (12 . 9) (9 . 12) (22 . 4) (4 . 22)
  ;;         (18 . 7) (7 . 18) (17 . 18) (18 . 17) (4 . 30) (30 . 4) (1 . 28) (28 . 1)
  ;;         (22 . 26) (26 . 22) (18 . 21) (21 . 18) (23 . 13) (13 . 23) (21 . 4) (4 . 21)
  ;;         (26 . 30) (30 . 26) (17 . 20) (20 . 17) (8 . 23) (23 . 8) (19 . 12) (12 . 19)
  ;;         (29 . 12) (12 . 29) (19 . 12) (12 . 19))
  (apply #'append (loop repeat *edge-num*
			collect (edge-pair (random-node) (random-node)))))

;; Preventing Islands
;; ==================

(defun direct-edges (node edge-list)
  ;; Function
  ;; ========
  ;;
  ;; Input:
  ;; =====
  ;;  [1] - 'node', a simple node.
  ;;  [2] - 'edge-list', the edge-list.
  ;;
  ;; Output:
  ;; ======
  ;;  Finds all the edges in 'edge-list' that start with 'node'. It does this by
  ;; creating a new list with all edges removed that don’t have the 'node' in the car
  ;; position.
  ;;
  ;; Run:
  ;; ===
  ;;  [1] > (direct-edges 21 '((5 . 11) (11 . 5) (24 . 21) (21 . 24) (15 . 11) (11 . 15) (6 . 21) (21 . 6)))
  ;;        ((21 . 24) (21 .26))
  ;;
  ;;  [2] > (direct-edges 3 '((5 . 11) (11 . 5) (24 . 21) (21 . 24) (15 . 11) (11 . 15) (6 . 21) (21 . 6)))
  ;;        NIL
  (remove-if-not (lambda (x)
		   (eql (car x) node))
		 edge-list))

(defun get-connected (node edge-list)
  ;; Function
  ;; ========
  ;;
  ;; Input:
  ;; =====
  ;;  [1] - 'node', a simple node
  ;;  [2] - 'edge-list', the edge-list
  ;;
  ;; Output:
  ;; ======
  ;;  To find islands, this function takes an 'edge-list' and a source 'node' and
  ;; builds a list of all nodes connected to that 'node', even if it requires walking
  ;; across multiple edges.
  ;;
  ;;
  ;; Run:
  ;; ===
  ;;  [1] > (get-connected 21 '((5 . 11) (11 . 5) (24 . 21) (21 . 24) (15 . 11) (11 . 15) (6 . 21) (21 . 6)))
  ;;        (6 24 21)
  ;;
  ;;  [2] > (get-connected 3 '((5 . 11) (11 . 5) (24 . 21) (21 . 24) (15 . 11) (11 . 15) (6 . 21) (21 . 6)))
  ;;        (3)
  (let ((visited nil))
    (labels ((traverse (node)
		       (unless (member node visited)
			 (push node visited)
			 (mapc (lambda (edge)
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
  ;;  [1] - 'nodes', list with all nodes
  ;;  [2] - 'edges-list', the edge-list
  ;;
  ;; Output:
  ;; ======
  ;;  Function that will find all the islands in our graph. First, its finds all
  ;; connected nodes and unconnected nodes, subtract them putting the unconnected
  ;;
  ;; Run:
  ;; ===
  ;;  [1] > (find-islands '(3 21 6 4) '((5 . 11) (11 . 5) (24 . 21) (21 . 24) (15 . 11) (11 . 15) (6 . 21) (21 . 6)))
  ;;        ((4) (6 24 21) (3))
  ;;
  ;;  [2] > (find-islands '(3 21 6 4) '((6 . 11) (11 . 6) (24 . 21) (21 . 24) (15 . 11) (11 . 15) (6 . 21) (21 . 6)))
  ;;        ((4) (15 11 6 24 21) (3))
  ;;
  ;;  [3] > (find-islands '(3 21 4 6 13 15 24) '((6 . 11) (11 . 6) (24 . 21) (21 . 24) (15 . 11) (11 . 15) (6 . 21) (21 . 6)))
  ;;        ((13) (4) (15 11 6 24 21) (3))
  ;;
  ;;  [4] > (find-islands '(21 6) '((5 . 11) (11 . 5) (24 . 21) (21 . 24) (15 . 11) (11 . 15) (6 . 21) (21 . 6)))
  ;;        ((6 24 21))
  (let ((islands nil))
    (labels ((find-island (nodes)
			  (let* ((connected (get-connected (car nodes) edge-list))
				 (unconnected (set-difference nodes connected)))
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
  ;;  [1] - 'islands',
  ;;
  ;; Output:
  ;; ======
  ;;  Returns a list of additional edges that join all the islands together.
  ;;
  ;; Run:
  ;; ===
  ;;  [1] > (connect-with-bridges '((4) (6 24 21) (3)))
  ;;        ((4 . 6) (6 . 4) (6 . 3) (3 . 6))
  ;;
  ;;  [2] > (connect-with-bridges '((13) (4) (15 11 6 24 21) (3)))
  ;;        ((13 . 4) (4 . 13) (4 . 15) (15 . 4) (15 . 3) (3 . 15))
  ;;
  ;;  [3] > (connect-with-bridges '((6 24 21)))
  ;;        NIL
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
  ;;  [1] - 'nodes', a list with all nodes
  ;;  [2] - 'edge-list', a list with all the edges
  ;;
  ;; Output:
  ;; ======
  ;;  It uses find-islands to find all the land masses, and then calls
  ;; connect-with-bridges to build appropriate bridges. It then appends these bridges
  ;; to the initial list of edges to produce a final, fully connected land mass.
  ;;
  ;; Run:
  ;; ===
  ;;  [1] > 
  (append (connect-with-bridges (find-islands nodes edge-list)) edge-list))

;; Building the inal Edges for Congestion City

(defun make-city-edges ()
  ;; Function
  ;; ========
  ;;
  ;; Output:
  ;; ======
  ;;  Creates a list of nodes, putting bllocks of cops in random nodes.
  ;;
  (let* ((nodes (loop for i from 1 to *node-num*
		      collect i))
	 (edge-list (connect-all-islands nodes (make-edge-list)))
	 (cops (remove-if-not (lambda (x)
				(zerop (random *cop-odds*)))
			      edge-list)))
    (add-cops (edges-to-alist edge-list) cops)))

(defun edges-to-alist (edge-list)
  (mapcar (lambda (node1)
            (cons node1
                  (mapcar (lambda (edge)
			    (list (cdr edge)))
			  (remove-duplicates (direct-edges node1 edge-list)
					     :test #'equal))))
	  (remove-duplicates (mapcar #'car edge-list))))

(defun add-cops (edge-alist edges-with-cops)
  (mapcar (lambda (x)
	    (let ((node1 (car x))
		  (node1-edges (cdr x)))
	      (cons node1
		    (mapcar (lambda (edge)
			      (let ((node2 (car edge)))
				(if (intersection (edge-pair node1 node2)
						  edges-with-cops
						  :test #'equal)
				    (list node2 'cops)
				  edge)))
			    node1-edges))))
	  edge-alist))

