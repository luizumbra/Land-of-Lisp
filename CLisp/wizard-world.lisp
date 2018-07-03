;; Wizard World
;; ============
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


;; Chapter 5:
;; =========
;;  Building a Text Game Engine
;;
;; Abstract:
;; ========
;;  In this chapter, we put together a simple engine for a text adventure game.
;; Along the way, you learned the following:
;;
;;    - A game world can be represented by a mathematical graph, consisting of
;;    nodes for the places the player can visit and edges for the paths between
;;    these places.
;;
;;    - You can store these nodes in an association list (alist) called *nodes*.
;;    This alist allows you to look up properties of a node/place by using its
;;    name. In the case of our game, the property we’re storing is a description
;;    of each node/place.
;;
;;    - You use the assoc function to look up a key (location name in our
;;    example) in an alist.
;;
;;    - Quasiquoting is a technique that allows you to insert small bits of
;;    computer code into larger pieces of data.
;;
;;    - Some Lisp functions accept other functions as arguments. These are called
;;    higher-order functions. The mapcar function is the most popular higherorder
;;    function in Common Lisp.
;;
;;    - To replace a value from an alist, you push new items onto the list. Only
;;    the most recent value will be reported by the assoc function.

(defparameter *nodes*
  ;; Global-parameter
  ;; ================
  ;;
  ;; Description:
  ;; ===========
  ;;  An alist to describe the game scenario.
  ;;  Each scenario will be represented as a node of a graph.
  '((living-room (you are in the living-room.
		      a wizard is snoring loudly on the couch.))
    (garden (you are in a beautiful garden.
		 there is a well in front of you.))
    (attic (you are in the attic.
		there is a giant welding torch in the corner.))))

(defun describe-location (location nodes)
  ;; Function
  ;; ========
  ;;
  ;; Input:
  ;; =====
  ;;  [1] - location, a symbol that indicate the current location player
  ;;  [2] - nodes, an alist with a location as key and it description.
  ;;
  ;; Output:
  ;; ======
  ;;  Find location in nodes and return the description of that node (the second
  ;;elemento of the alist).
  (cadr (assoc location nodes)))


(defparameter *edges*
  ;; Global-parameter
  ;; ================
  ;;
  ;; Description:
  ;; ===========
  ;;  An alist to walk between the scenes.
  ;;  Each element contain the scenario node and the path to another scenario
  ;; node.
  '((living-room (garden west door)
		 (attic upstairs ladder))
    (garden (living-room east door))
    (attic (living-room downstairs ladder))))


(defun describe-path (edge)
  ;; Function:
  ;; ========
  ;;
  ;; Input:
  ;; =====
  ;;  [1] - edge, a list to represent the game map
  ;;
  ;; Output:
  ;; ======
  ;;  A function to describe the paths of *nodes*
  `(there is a ,(caddr edge) going ,(cadr edge) from here.))


(defun describe-paths (location edges)
  ;; Function
  ;; ========
  ;;
  ;; Input:
  ;; =====
  ;;  [1] - location, a symbol that represent the current location player.
  ;;  [2] - edges, a list of paths.
  ;;
  ;; Output:
  ;; ======
  ;;  Describe all the paths of the location symbol in edges.
  (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))


(defparameter *objects*
  ;; Global-parameter
  ;; ================
  ;;
  ;; Description:
  ;; ===========
  ;;  A List of visible objects, to be collect by the player.
  '(whiskey bucket frog chain))


(defparameter *object-locations*
  ;; Global-parameter
  ;; ================
  ;;
  ;; Description:
  ;; ===========
  ;;  A track o location of each visible object.
  '((whiskey living-room) (bucket living-room)
    (chain garden)
    (frog garden)))


(defun objects-at (loc objs obj-locs)
  ;; Function:
  ;; ========
  ;;
  ;; Input:
  ;; ====
  ;;  [1] - loc, a symbol of the current location player
  ;;  [2] - objs, a list that contain the visible objects
  ;;  [3] - obj-locs, a alist with a visible object as a key to guard the
  ;;      location of it.
  ;;
  ;; Output:
  ;; ======
  ;;  A list with the objects at the current location of the player.
  (labels ((at-loc-p (obj)
		     ;; Local-function:
		     ;; ==============
		     ;;
		     ;; Input:
		     ;; =====
		     ;; [1] - obj, a symbol as a visible object.
		     ;;
		     ;; Output:
		     ;; ======
		     ;;  The current location of the obj.
		     (eq (cadr (assoc obj obj-locs)) loc)))
	  (remove-if-not #'at-loc-p objs)))


(defun describe-objects (loc objs obj-loc)
  ;; Function:
  ;; ========
  ;;
  ;; Input:
  ;; =====
  ;; [1] - loc, a symbol of the current location player.
  ;; [2] - objs, a list of visible objects
  ;; [3] - obj-loc, a alist with a visible object as a key to the location.
  ;;
  ;; Output:
  ;; ======
  ;;  Describe where is the object, it is depend of the current location.
  (labels ((describe-obj (obj)
			 ;; Local-function:
			 ;; ==============
			 ;;
			 ;; Input:
			 ;; =====
			 ;;  [1] - obj, a symbol that will represent a visible
			 ;;      object.
			 ;;
			 ;; Output:
			 ;; ======
			 ;;  A List inform the objects of the current location
			 ;; player are.
			 `(you see a ,obj on the floor.)))
	  (apply #'append (mapcar #'describe-obj (objects-at loc objs obj-loc)))))


(defparameter *location*
  ;; Global-parameter
  ;; ================
  ;;
  ;; Description:
  ;; ===========
  ;;  The current location of the player, initialized with the living-room
  ;; location.
  'living-room)


(defun look ()
  ;; Function:
  ;; ========
  ;;
  ;; Output:
  ;; ======
  ;;  Describe the current location of the player with the possible path and
  ;; objects.
  (append (describe-location *location* *nodes*)
	  (describe-paths *location* *edges*)
	  (describe-objects *location* *objects* *object-locations*)))


(defun walk (direction)
  ;; Function
  ;; ========
  ;;
  ;; Input:
  ;; =====
  ;;  [1] - direction, a symbol to inform the next node that the player want to
  ;; go.
  ;;
  ;; Output:
  ;; ======
  ;;  Change the current location player (*location*). Case direction is not
  ;; availible inform the layer.
  (let ((next (find direction
		    (cdr (assoc *location* *edges*))
		    :key #'cadr)))
    (if next
	(progn (setf *location* (car next))
	       (look))
      '(you cannot go that way.))))


(defun pickup (object)
  ;; Function:
  ;; ========
  ;;
  ;; Input:
  ;; =====
  ;;  [1] - object, a symbol that represent the object on the floor of the
  ;;      current location.
  ;; Outout:
  ;; ======
  ;;  Change the location of a valid object at tha alist *object-location* to
  ;; 'body
  ;;
  ;; Error:
  ;; =====
  ;;  [1] - This not remove the picked object of the location. So we have to
  ;; think how to remove it.
  (cond ((member object
		 (objects-at *location* *objects* *object-locations*))
	 (push (list object 'body) *object-locations*)
         `(you are now carrying the ,object))
        (t '(you cannot get that.))))


(defun inventory ()
  ;; Function:
  ;; ========
  ;;
  ;; Output:
  ;; ======
  ;;  All objects piked up by the player.
  (cons 'items- (objects-at 'body *objects* *object-locations*)))


;; Chapter 6:
;; =========
;;
;;  Interacting with the World: Reading and Printing in Lisp.
;;
;; Abstract:
;; ========
;;  In this chapter, we created a custom REPL to supercharge our text adventure
;; game. Along the way, you learned the following:
;;
;;  - The print and read functions let you directly communicate with the user
;;  through the console. These two functions work in a computer-friendly way.
;;
;;  - Other input/output functions are not as elegant as read and print, but are
;;  friendlier for interacting with humans. Examples include princ and read-line.
;;
;;  - A homoiconic programming language stores its program code and program data
;;  in a similar format. Lisp’s quoting, quasiquoting, eval, and macro features
;;  make it extremely homoiconic.
;;
;;  - It’s easy to write your own custom REPL.
;;
;;  - It’s simple to transform your internal Lisp data into the format most
;;  suitable for your program’s interface. This makes it easy to separate
;;  presentation details from your program’s internal data structures.


(defun say-hello ()
  ;; Function:
  ;; ========
  ;;
  ;; Input:
  ;; =====
  ;;  [1] - Take the user name.
  ;;
  ;; Output:
  ;; ======
  ;;  Give a greteens to the player
  (princ "Please type your name:")
  (let ((name (read-line)))
    (princ "Nice to meet you, ")
    (princ name)))

(defun game-read ()
  ;; Function
  ;; ========
  ;;
  ;; Output:
  ;; ======
  ;;  Read the player command and put it as a converted Lisp syntax expression.
  (let ((cmd (read-from-string
	      (concatenate 'string "(" (read-line) ")"))))
    (flet ((quote-it (x)
		     ;; Local-Function
		     ;; ==============
		     ;;
		     ;; Input:
		     ;; =====
		     ;;  [1] - x, a command player.
		     ;;
		     ;; Output:
		     ;; ======
		     ;;  Put a quot at x.
		     (list 'quote x)))
	  (cons (car cmd)
		(mapcar #'quote-it (cdr cmd))))))


(defparameter *allowed-commands*
  ;; Global-parameter
  ;; ================
  ;;
  ;; Description:
  ;; ===========
  ;;  It is a list to know wich command is allowed to the player.
  '(look walk pickup inventory))

(defun game-eval (sexp)
  ;; Function
  ;; ========
  ;;
  ;; Input:
  ;; =====
  ;;  [1] - sexp, a valid player command
  ;;
  ;; Output:
  ;; ======
  ;;  Aplly eval to sexp, if that is a valid command. In case of sexp is not a
  ;; valid command give a warning messege.
  (if (member (car sexp) *allowed-commands*)
      (eval sexp)
    '(i do not know that command.)))

(defun tweak-text (lst caps lit)
  ;; Function
  ;; ========
  ;;
  ;; Input:
  ;; =====
  ;;  [1] - lst, a simbol-based writing
  ;;  [2] - caps, a flag to indicate if the next character must be in capital
  ;;      latter.
  ;;  [3] - lit, if it is a literal.
  ;;
  ;; Output:
  ;; ======
  ;;  Convert lst to a valid string.
  (when lst
    (let ((item (car lst))
	  (rest (cdr lst)))
      (cond ((eq item #\space)
	     (cons item (tweak-text rest caps lit)))
	    ((member item '(#\! #\? #\.))
	     (cons item (tweak-text rest t lit)))
	    ((eq item #\")
	     (tweak-text rest caps (not lit)))
	    (lit (cons item (tweak-text rest nil lit)))
	    ((or caps lit)
	     (cons (char-upcase item)
		   (tweak-text rest nil lit)))
	    (t (cons (char-downcase item)
		     (tweak-text rest nil nil)))))))


(defun game-print (lst)
  ;; Function
  ;; ========
  ;;
  ;; Input:
  ;; =====
  ;;  [1] - lst, a simbol-based writing
  ;;
  ;; Output:
  ;; ======
  ;;  Converts lst into a properly capitalized text.
  (princ (coerce (tweak-text (coerce (string-trim "() "
						  (prin1-to-string lst))
				     'list)
			     t
			     nil)
		 'string))
  (fresh-line))


(defun game-repl ()
  ;; Function
  ;; ========
  ;;
  ;; Output:
  ;; ======
  ;;  Initialize the game.
  (let ((cmd (game-read)))
    (unless (eq (car cmd) 'quit)
      (game-print (game-eval cmd))
      (game-repl))))

