;;;; -*- Guess-My-Number Game -*-
;;; @author: Conrad Barsli, M. D.
;;; @book: Land of Lisp: Learn to program in Lisp, one game at a time!
;;; @date: 2011
;;;; -*- Copyright C 2011 by Conrad Barski, M.D. -*-

(defparameter *nodes*
  "Alist representing the nodes of the world with a simple description. Each node has the following structure: (location description)."
  '((living-room (you are in the living-room.
		      a wizard is snoring loudly on the couch.))
    (garden (you are in a beautiful garden.
		 there is a well in front of you.))
    (attic (you are in the attic.
		there is a giant welding torch in the corner.))))

(defun describe-location (location nodes)
  "A simple function to show the surrounding of a determined location. It will use location as a key to find the description inside node.

Args:
  location (symbol): symbol that represent one location of the world.
  nodes (alist): alist representing the world graph. Its use *nodes* as default.

Return:
  (symbol): Return the description of location inside nodes."
  (cadr (assoc location nodes)))

(defparameter *edges*
  "List representing the world edges graph. Each edge has the following structure: ((location-key (location-connection direction access-to-location-conection) ...) ...)"
  '((living-room (garden west door)  
                 (attic upstairs ladder))
    (garden (living-room east door))
    (attic (living-room downstairs ladder))))

(defun describe-path (edge)
  "Using an edge from *edges* it describe to the user where and how to go to a conect location.

Args:
  edge (list): an edge of *edges* with the following structure: (location-conection direction access-to-location-conection).

Return:
  (list): Return a simple description of how to go to a determined location."
  `(there is a ,(caddr edge) going ,(cadr edge) from here.))

(defun describe-paths (location edges)
  "Describe all the paths of current location.

Args:
  location (symbol): symbol that represent the player current location.
  edges (list): list representing the connections between location. Its uses *edges* as default.

Return:
  (list): Return a simple description of possible paths from the current location using describe-paths."
  (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))

(defparameter *objects*
  "List with the symbols that represent the collectable objects."
  '(whiskey bucket frog chain))

(defparameter *object-locations* '((whiskey living-room)
                                   (bucket living-room)
                                   (chain garden)
                                   (frog garden)))

(defun objects-at (loc objs obj-loc)
  (labels ((is-at (obj)
		  (eq (cadr (assoc obj obj-loc)) loc)))
	  (remove-if-not #'is-at objs)))

(defun describe-objects (loc objs obj-loc)
  (labels ((describe-obj (obj)
			 `(you see a ,obj on the floor.)))
	  (apply #'append (mapcar #'describe-obj (objects-at loc objs obj-loc)))))

(defparameter *location* 'living-room)

(defun look ()
  (append (describe-location *location* *nodes*)
          (describe-paths *location* *edges*)
          (describe-objects *location* *objects* *object-locations*)))

(defun walk (direction)
  (labels ((correct-way (edge)
			(eq (cadr edge) direction)))
	  (let ((next (find-if #'correct-way (cdr (assoc *location* *edges*)))))
	    (if next 
		(progn (setf *location* (car next)) 
                       (look))
              '(you cannot go that way.)))))

(defun pickup (object)
  (cond ((member object (objects-at *location* *objects* *object-locations*))
         (push (list object 'body) *object-locations*)
         `(you are now carrying the ,object))
	(t '(you cannot get that.))))

(defun inventory ()
  (cons 'items- (objects-at 'body *objects* *object-locations*)))

(defun have (object) 
  (member object (cdr (inventory))))

(defun game-repl ()
  (let ((cmd (game-read)))
    (unless (eq (car cmd) 'quit)
      (game-print (game-eval cmd))
      (game-repl))))

(defun game-read ()
  (let ((cmd (read-from-string (concatenate 'string "(" (read-line) ")"))))
    (flet ((quote-it (x)
                     (list 'quote x)))
          (cons (car cmd) (mapcar #'quote-it (cdr cmd))))))

(defparameter *allowed-commands* '(look walk pickup inventory))

(defun game-eval (sexp)
  (if (member (car sexp) *allowed-commands*)
      (eval sexp)
    '(i do not know that command.)))

(defun tweak-text (lst caps lit)
  (when lst
    (let ((item (car lst))
          (rest (cdr lst)))
      (cond ((eql item #\space) (cons item (tweak-text rest caps lit)))
            ((member item '(#\! #\? #\.)) (cons item (tweak-text rest t lit)))
            ((eql item #\") (tweak-text rest caps (not lit)))
            (lit (cons item (tweak-text rest nil lit)))
            (caps (cons (char-upcase item) (tweak-text rest nil lit)))
            (t (cons (char-downcase item) (tweak-text rest nil nil)))))))

(defun game-print (lst)
  (princ (coerce (tweak-text (coerce (string-trim "() " (prin1-to-string lst)) 'list) t nil) 'string))
  (fresh-line))
