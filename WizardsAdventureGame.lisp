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
(defparameter *node* '((living-room (you are in the living-room. a wizard is snoring
					 loudly on the couch.))
		       (garden (you are in a beatiful garden. there is a well in
				    front of you.))
		       (attic (you are in the attic. there is a giant welding torch
				   in the corner.))))

;; describe-location
;; =================
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

;; describe-path
;; =============
;;
;; Description:
;; ============
;;  describe-path function builds a textual description of a given edge using our
;; symbols system.
(defun describe-path (edge)
  `(there is a ,(caddr edge) going ,(cadr edge) from here.))

;; describe-paths
;; ==============
;;
;; Description:
;; ============
;;  It is a function that can generate descriptions for all edges from a given
;; location by looking up the location from our data structure of edges.
(defun describe-paths (location edges)
  (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))

