;; "Guess my number"
;;  ===============
;;
;; Description:
;; ===========
;;   Program of the book "Land of Lisp". In this program the user havo to choose a
;; number between 1 to 100, and the program have to guess wich number was choosed by
;; the user using binary search.
;;
;; Copyright
;; =========
;;   Author of the book "Land of Lisp"

;; Global variables

;; *small*
;; Smaller number to guess
(defparameter *small* 1)

;; *big*
;; Bigger number to guess
(defparameter *big* 100)

;; Global function, related only with the game

;; guess-my-number
;; @return a number guessed from the *small* and *big* variables
(defun guess-my-number ()
  (ash (+ *small* *big*) -1))

;; smaller
;; @return a smaller guess number
(defun smaller ()
  (setf *big* (+ (guess-my-number) -1))
  (guess-my-number))

;; bigger
;; @return a bigger guess number
(defun bigger ()
  (setf *small* (+ (guess-my-number) 1))
  (guess-my-number))

;; start-over
;; @return both, *small* and *big*, to original value of them
(defun start-over ()
  (setf *big* 100)
  (setf *small* 1)
  (guess-my-number))
