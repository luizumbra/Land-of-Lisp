;; Wizard's Adventure Game
;; =======================
;;
;;


;; *nodes*
;; =======
;;
;; Description:
;; ============
;;  *nodes* is a top-level global variable that contain descritions of the locations
;; that exist in our game. With that format (key (key-description)).
(defparameter *nodes* '((living-room (you are in the living-room. a wizard is snoring
					 loudly on the couch.))
		       (garden (you are in a beatiful garden. there is a well in
				    front of you.))
		       (attic (you are in the attic. there is a giant welding torch
				   in the corner.))))

;; describe-location()
;; ===================
;;
;; Description:
;; ============
;;  It is a function that describe a location in the game.
(defun describe-location (location nodes)
  (cadr (assoc location nodes)))

;; *edges*
;; =======
;;
;; Descrition:
;; ===========
;;  *edges* is a top-level global variable that contain the path to each location.
;; With that format (key (new-location direction use-to))
(defparameter *edges* '((living-room (garden west door)
				     (attic upstairs ladder))
			(garden (living-room east door))
			(attic (living-room downstairs ladder))))

;; describe-path()
;; ===============
;;
;; Description:
;; ============
;;  describe-path function builds a textual description of a given edge using our
;; symbols system.
(defun describe-path (edge)
  `(there is a ,(caddr edge) going ,(cadr edge) from here.))

;; describe-paths()
;; ================
;;
;; Description:
;; ============
;;  It is a function that can generate descriptions for all edges from a given
;; location by looking up the location from our data structure of edges.
(defun describe-paths (location edges)
  (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))

;; *objects*
;; =========
;;
;; Description:
;; ============
;;  A list of objects on the floor at a given location, wich the player can pick up
;; and use.
(defparameter *objects* '(whiskey bucket frog chain))

;; *object-locations*
;; ==================
;;
;; Description:
;; ============
;;  Variable to track the location of each object in form of an alist:
;; (*object* *location*).
(defparameter *object-locations* '((whiskey living-room)
				   (bucket living-room)
				   (chain garden)
				   (frog garden)))

;; object-at()
;; ===========
;;
;; Description:
;; ============
;;  A function that lists the objects visible from a given location.
(defun objects-at (loc objs obj-locs)
  (labels ((at-loc-p (obj)
		     (eq (cadr (assoc obj obj-locs)) loc)))
	  (remove-if-not #'at-loc-p objs)))

;; describe-objects()
;; ==================
;;
;; Description:
;; ============
;;  A function to describe the objects visible at a given location.
(defun describe-objects (loc objs obj-loc)
  (labels ((describe-obj (obj)
			 `(you see a ,obj on the floor.)))
	  (apply #'append (mapcar #'describe-obj (objects-at loc objs obj-loc)))))

;; *location*
;; ==========
;;
;; Description:
;; ============
;;  Variable to track the player's current position.
(defparameter *location* 'living-room)

;; look()
;; ======
;;
;; Description:
;; ============
;;  A function to describe everything we need by having it call all of our descriptor
;; functions.
(defun look ()
  (append (describe-location *location* *nodes*)
	  (describe-paths *location* *edges*)
	  (describe-objects *location* *objects* *object-locations*)))

;; walk()
;; ======
;;
;; Description:
;; ============
;;  The walk function takes a direction and lets us walk there.
(defun walk (direction)
  (let ((next (find direction
		    (cdr (assoc *location* *edges*))
		    :key #'cadr)))
    (if next
	(progn (setf *location* (car next))
	       (look))
      '(you cannot go that way.))))

;; pickup()
;; ========
;;
;; Description:
;; ============
;;  Command to pick up a object.
(defun pickup (object)
  (cond ((member object
		 (objects-at *location* *objects* *object-locations*))
	 (push (list object 'body) *object-locations*)
	 `(you are now carrying the ,object))
	(t '(you cannot get that.))))

;; inventory()
;; ===========
;;
;; Description
;; ===========
;;  A function that lets players see an inventory.
(defun inventory ()
  (cons 'items- (objects-at 'body *objects* *object-locations*)))

