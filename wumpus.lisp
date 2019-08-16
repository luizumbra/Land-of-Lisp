;;;; -*- Guess-My-Number Game -*-
;;; @author: Conrad Barsli, M. D.
;;; @book: Land of Lisp: Learn to program in Lisp, one game at a time!
;;; @date: 2011
;;;; -*- Copyright C 2011 by Conrad Barski, M.D. -*-

(load "graph-util")

(defparameter *congestion-city-nodes* nil
  "Variable to store the nodes of the game world.")
(defparameter *congestion-city-edges* nil
  "Variable to store the edges of the game world.")
(defparameter *visited-nodes* nil
  "Variable to store and inform the player visited places.")

(defparameter *node-num* 30
  "Maximum number of Congestion City location.")
(defparameter *edge-num* 45
  "Maximum number of roads of Congestion City.")
(defparameter *worm-num* 3
  "Total number of Gruessome Glowworm Gang in the city.")
(defparameter *cop-odds* 15
  "Total number of Cops in the city.")

(defun random-node ()
  "Generate a random number for a especific location.

  Return:
    (integer): a random number between 1 and *node-num*."
  (1+ (random *node-num*)))

(defun edge-pair (a b)
  "Create two directed edges between the selected nodes.

  Args:
    a (integer): a integer representing location.
    b (integer): a integer representing location.

  Return:
    (list): a list that contain two alist representing the connection between a to b and b to a."
  (unless (eql a b)
    (list (cons a b) (cons b a))))

(defun make-edge-list ()
  "Generates the actual list of random roads.

  Return:
    (list): a list with *edge-num* elements. Each elements it is a call of the function edge-pair with random parameters."
  (apply #'append (loop repeat *edge-num*
		     collect (edge-pair (random-node) (random-node)))))

(defun direct-edges (node edge-list)
  "Finds all the edges in an edge list that start from a given node.

  Args:
    node (integer): a node world that must be a direct node.
    edge-list (list): a list that contain the edges of the world.

  Return:
    (list): collect all edges that do not have node on car position."
  (remove-if-not (lambda (x)
                   (eql (car x) node))
                 edge-list))

(defun get-connected (node edge-list)
  "Finds the nodes already connected.

  Args:
    node (integer): a source node.
    edge-list (list): a list of edges.

  Return:
    (list): all node connect to the source node inside edge-list."
  (let ((visited nil))
    (labels ((traverse (node)
               (unless (member node visited)
                 (push node visited)
                 (mapc (lambda (edge)
                         (traverse (cdr edge)))
                       (direct-edges node edge-list)))))
      (traverse node))
    visited))

(defun connect-with-bridges (islands)
  "Bridging the islands together.

  Args:
    islands (list): list of nodes.

  Return:
    (list): Conncet all the bridges with the edge-pair function."
  (print islands)
  (when (cdr islands)
    (append (edge-pair (caar islands) (caadr islands))
            (connect-with-bridges (cdr islands)))))

(defun find-islands (nodes edge-list)
  "Generate a list of islands nodes.

  Args:
    nodes (list): nodes list.
    edge-list: list of edges.

  Return:
    (list): subtracts the connected nodes with all nodes."
  (let ((islands nil))
    (labels ((find-island (nodes)
               (let* ((connected (get-connected (car nodes) edge-list))
                      (unconnected (set-difference nodes connected)))
                 (push connected islands)
                 (when unconnected
                   (find-island unconnected)))))
      (find-island nodes))
    islands))

(defun connect-all-islands (nodes edge-list)
  "Append the conections of islands nodes with edge-list.

  Args:
    nodes (list): node list.
    edge-list (list): list of edges.

  Return:
    (list): connect the islands together them append to edge-list."
  (append (connect-with-bridges (find-islands nodes edge-list)) edge-list))

(defun edges-to-alist (edge-list)
  ""
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

(defun make-city-edges ()
  (let* ((nodes (loop for i from 1 to *node-num*
		   collect i))
         (edge-list (connect-all-islands nodes (make-edge-list)))
         (cops (remove-if-not (lambda (x)
                                (zerop (random *cop-odds*)))
                              edge-list)))
    (add-cops (edges-to-alist edge-list) cops)))

(defun neighbors (node edge-alist)
  (mapcar #'car (cdr (assoc node edge-alist))))

(defun within-one (a b edge-alist)
  (member b (neighbors a edge-alist)))

(defun within-two (a b edge-alist)
  (or (within-one a b edge-alist)
      (some (lambda (x)
              (within-one x b edge-alist))
            (neighbors a edge-alist))))

(defun make-city-nodes (edge-alist)
  (let ((wumpus (random-node))
        (glow-worms (loop for i below *worm-num*
		       collect (random-node))))
    (loop for n from 1 to *node-num*
       collect (append (list n)
		       (cond ((eql n wumpus) '(wumpus))
			     ((within-two n wumpus edge-alist) '(blood!)))
		       (cond ((member n glow-worms)
			      '(glow-worm))
			     ((some (lambda (worm)
				      (within-one n worm edge-alist))
				    glow-worms)
			      '(lights!)))
		       (when (some #'cdr (cdr (assoc n edge-alist)))
			 '(sirens!))))))

(defun new-game ()
  (setf *congestion-city-edges* (make-city-edges))
  (setf *congestion-city-nodes* (make-city-nodes *congestion-city-edges*))
  (setf *player-pos* (find-empty-node))
  (setf *visited-nodes* (list *player-pos*))
  (draw-city))

(defun find-empty-node ()
  (let ((x (random-node)))
    (if (cdr (assoc x *congestion-city-nodes*))
        (find-empty-node)
        x)))

(defun draw-city ()
  (ugraph->png "city" *congestion-city-nodes* *congestion-city-edges*))

(defun known-city-nodes ()
  (mapcar (lambda (node)
            (if (member node *visited-nodes*)
                (let ((n (assoc node *congestion-city-nodes*)))
                  (if (eql node *player-pos*)
                      (append n '(*))
                      n))
                (list node '?)))
          (remove-duplicates 
	   (append *visited-nodes*
		   (mapcan (lambda (node)
			     (neighbors node *congestion-city-edges*))
			   *visited-nodes*)))))

(defun known-city-edges ()
  (mapcar (lambda (node)
            (cons node (mapcar (lambda (x)
                                 (if (member (car x) *visited-nodes*)
                                     x
				     (list (car x))))
			       (cdr (assoc node *congestion-city-edges*)))))
          *visited-nodes*))

(defun draw-known-city ()
  (ugraph->png "known-city" (known-city-nodes) (known-city-edges)))

(defun new-game ()
  (setf *congestion-city-edges* (make-city-edges))
  (setf *congestion-city-nodes* (make-city-nodes *congestion-city-edges*))
  (setf *player-pos* (find-empty-node))
  (setf *visited-nodes* (list *player-pos*))
  (draw-city)
  (draw-known-city))

(defun walk (pos)
  (handle-direction pos nil))

(defun charge (pos)
  (handle-direction pos t))

(defun handle-direction (pos charging)
  (let ((edge (assoc pos 
                     (cdr (assoc *player-pos* *congestion-city-edges*)))))
    (if edge
        (handle-new-place edge pos charging)
	(princ "That location does not exist!"))))

(defun handle-new-place (edge pos charging)
  (let* ((node (assoc pos *congestion-city-nodes*))
         (has-worm (and (member 'glow-worm node)
                        (not (member pos *visited-nodes*)))))
    (pushnew pos *visited-nodes*)
    (setf *player-pos* pos)
    (draw-known-city)
    (cond ((member 'cops edge) (princ "You ran into the cops. Game Over."))
          ((member 'wumpus node) (if charging
                                     (princ "You found the Wumpus!")
                                     (princ "You ran into the Wumpus")))
          (charging (princ "You wasted your last bullet. Game Over."))
          (has-worm (let ((new-pos (random-node)))
                      (princ "You ran into a Glow Worm Gang! You're now at ")
                      (princ new-pos)
                      (handle-new-place nil new-pos nil))))))
