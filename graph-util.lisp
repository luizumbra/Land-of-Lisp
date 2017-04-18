;; Graph-Test
;; ==========
;;
;; Land of Lisp
;; ============
;;
;; Author:
;; ======
;;  Barski, C. "Land of Lisp: Learn to Program in Lisp, One Game at a Time!",
;; 2011.
;;
;; Bibliography:
;; ============
;;  [1] Uses the Open Source code of Graphviz, in URL http://www.graphviz.org,
;; last access at April 11, 2017.
;;
;; Copyright © 2011 by Conrad Barski, M.D.


;; Chapter 7:
;; =========
;;  GOING BEYOND BASIC LISTS
;;
;; Abstract:
;; ========
;;  In this chapter, we discussed exotic types of lists and created a drawing
;; library for mathematical graphs. Along the way, you learned the following:
;;
;;  - You can create lists in Lisp that end in a value other than nil. Such lists
;;  are displayed with an extra dot before the last item and are called dotted
;;  lists.
;;
;;  - Pairs are what you get when you cons together two items that are not lists
;;  themselves. They can also be thought of as dotted lists that contain only
;;  two items.
;;
;;  - Circular lists are lists where the last cons cell points to an earlier cons
;;  cell in the same list.
;;
;;  - Association lists (alists) are lists of pairs. They can be used to store
;;  data that is in the form of keys associated with values.
;;
;;  - Lisp syntax expressions are great for storing and visualizing list-like and
;;  hierarchical data. Extra tools may be helpful for visualizing more complex
;;  data.
;;
;;  - If your data is in the form of a mathematical graph, it’s helpful to be
;;  able to generate pictures of your data using Graphviz.
;;
;;  - A common technique for generating textual data in a Lisp program is to
;;  write functions that print the text to the console for easy debugging and
;;  wrap these functions in thunks. Then you can send these thunks to other
;;  functions, which capture the console output and route the text to the
;;  appropriate destination, such as writing it to a file.


(defun dot-name (exp)
  ;; Function
  ;; ========
  ;;
  ;; Input:
  ;; =====
  ;;  [1] - exp, a expression to represent a identifier of a DOT file.
  ;;
  ;; Output:
  ;; ======
  ;;  A node in DOT format can contain only letters, digits, and the underscore
  ;; character. To make sure the node identifier we’re using is legal, we’ll
  ;; change any forbidden characters to underscores.
  (substitute-if #\_ (complement #'alphanumericp)
		 (prin1-to-string exp)))

(defparameter *max-label-length*
  ;; Global-parameter
  ;; ================
  ;;
  ;; Description:
  ;; ===========
  ;;  A value to determine the maximum number of characters for a label.
  30)

(defun dot-label (exp)
  ;; Function
  ;; ========
  ;;
  ;; Input:
  ;; =====
  ;;  [1] - exp, a DOT label
  ;;
  ;; Output:
  ;; ======
  ;;  Generate the label that should appear in the node when it is drawn. The
  ;; label will consist of the node name and the data linked to the node in the
  ;; node alist.
  (if exp
      (let ((s (write-to-string exp :pretty nil)))
	(if (> (length s) *max-label-length*)
	    (concatenate 'string (subseq s 0 (- *max-label-length* 3)) "...") s))
    ""))

(defun nodes->dot (nodes)
  ;; Function:
  ;; ========
  ;;
  ;; Input:
  ;; =====
  ;;  [1] - nodes, an alist of nodes
  ;;
  ;; Output:
  ;; ======
  ;;  Takes nodes and generates the DOT information that encodes them.
  (mapc (lambda (node)
	  (fresh-line)
	  (princ (dot-name (car node)))
          (princ "[label=\"")
          (princ (dot-label node))
          (princ "\"];"))
	nodes))

(defun edges->dot (edges)
  ;; Function
  ;; ========
  ;;
  ;; Input:
  ;; =====
  ;;  [1] - edges, a alist with the link between the nodes and the description.
  ;;
  ;; Output:
  ;; ======
  ;;  Generates the necessary data, again by printing it directly to the console.
  (mapc (lambda (node)
	  (mapc (lambda (edge)
		  (fresh-line)
		  (princ (dot-name (car node)))
		  (princ "->")
		  (princ (dot-name (car edge)))
		  (princ "[label=\"")
		  (princ (dot-label (cdr edge)))
		  (princ "\"];"))
		(cdr node)))
	edges))

(defun graph->dot (nodes edges)
  ;; Function
  ;; ========
  ;;
  ;; Input:
  ;; =====
  ;;  [1] - nodes, the nodes with the description
  ;;  [2] - edges, the edges link of nodes with the description
  ;;
  ;; Output:
  ;; ======
  ;;  Complete the generation of the DOT data, we call both nodes->dot and
  ;; edges->dot, and wrap it up with some extra decoration.
  (princ "digraph{")
  (nodes->dot nodes)
  (edges->dot edges)
  (princ "}"))

(defun dot->png (fname thunk)
  ;; Function
  ;; ========
  ;;
  ;; Input:
  ;; =====
  ;;  [1] - fname, a name to give the dot file
  ;;  [2] - thunk, the grph-dot function name
  ;;
  ;; Output:
  ;; ======
  ;;  Capture the DOT file data, put it into a file, and then execute the dot
  ;; command directly from the system command line
  (with-open-file (*standard-output*
		   fname
		   :direction :output
		   :if-exists :supersede)
		  (funcall thunk))
  (ext:shell (concatenate 'string "dot -Tpng -O " fname)))

(defun graph->png (fname nodes edges)
  ;; Function
  ;; ========
  ;;
  ;; Input:
  ;; =====
  ;;  [1] - fname, name of a DOT file
  ;;  [2] - nodes, the graph nodes
  ;;  [3] - edges, the graph edges
  ;;
  ;; Output:
  ;; ======
  ;;  Function that ties together all the pieces to let us easily create a graph
  ;; from some nodes and edges.
  (dot->png fname
            (lambda ()
              (graph->dot nodes edges))))

;; Undirected Graphs

(defun uedges->dot (edges)
  ;; Function
  ;; ========
  ;;
  ;; Input:
  ;; =====
  ;;  [1] - edges, a alist with the link between the nodes and the description.
  ;;
  ;; Output:
  ;; ======
  ;;  Generates the necessary data, again by printing it directly to the console.
  (maplist (lambda (lst)
	     (mapc (lambda (edge)
		     (unless (assoc (car edge) (cdr lst))
		       (fresh-line)
		       (princ (dot-name (caar lst)))
		       (princ "--")
		       (princ (dot-name (car edge)))
		       (princ "[label=\"")
		       (princ (dot-label (cdr edge)))
		       (princ "\"];")))
		   (cdar lst)))
	   edges))

(defun ugraph->dot (nodes edges)
  ;; Function
  ;; ========
  ;;
  ;; Input:
  ;; =====
  ;;  [1] - nodes, the nodes with the description
  ;;  [2] - edges, the edges link of nodes with the description
  ;;
  ;; Output:
  ;; ======
  ;;  Complete the generation of the DOT data, we call both nodes->dot and
  ;; edges->dot, and wrap it up with some extra decoration.  
  (princ "graph{")
  (nodes->dot nodes)
  (uedges->dot edges)
  (princ "}"))

(defun ugraph->png (fname nodes edges)
  ;; Function
  ;; ========
  ;;
  ;; Input:
  ;; =====
  ;;  [1] - fname, name of a DOT file
  ;;  [2] - nodes, the graph nodes
  ;;  [3] - edges, the graph edges
  ;;
  ;; Output:
  ;; ======
  ;;  Function that ties together all the pieces to let us easily create a graph
  ;; from some nodes and edges.  
  (dot->png fname
	    (lambda ()
	      (ugraph->dot nodes edges))))

