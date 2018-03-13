;; Land of Lisp
;; ============
;;
;; Game 1 - Guess
;; ==============
;;  In this game, you pick a number from 1 to 100, and the computer has to guess it.

;; Global variable is called as "top-level definition. Lispers use asterisks
;; surrounding the global variables names - affectionately called earmuffs - to
;; diferentiate with locals varibles.
(defparameter *small*
  ;; Global variable
  ;; ===============
  ;;  Define the smallest number guessed.
  1)

(defparameter *big*
  ;; Global variable
  ;; ===============
  ;;  Define the largest number guessed.
  100)



(defun guess-my-number ()
     (ash (+ *small* *big*) -1))

(defun smaller ()
     (setf *big* (1- (guess-my-number)))
     (guess-my-number))

(defun bigger ()
     (setf *small* (1+ (guess-my-number)))
     (guess-my-number))

(defun start-over ()
   (defparameter *small* 1)
   (defparameter *big* 100)
   (guess-my-number))
