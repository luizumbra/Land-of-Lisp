;;;; -*- Guess-My-Number Game -*-
;;; @author: Conrad Barsli, M. D.
;;; @book: Land of Lisp: Learn to program in Lisp, one game at a time!
;;; @date: 2011
;;;; -*- Copyright C 2011 by Conrad Barski, M.D. -*-

(defparameter *small* 1
  "A paremeter that represent the possible smallest value.")
(defparameter *big* 100
  "A parameter that represent the possible larger value.")

(defun guess-my-number ()
  "It will half between the bigger and smalest possible number to guess the user number."
  (ash (+ *small* *big*) -1))

(defun smaller ()
  "Function to the user inform that his number is smaller than guessed. It will change the biggest possible number between it and the samlest."
  (setf *big* (1- (guess-my-number)))
  (guess-my-number))

(defun bigger ()
  "Function to the user inform that his number is bigger than guessed. It will change the smalest possible number between it and the biggest."
  (setf *small* (1+ (guess-my-number)))
  (guess-my-number))

(defun start-over ()
  "Reset command. It will put all the variables to its origin values."
  (defparameter *small* 1)
  (defparameter *big* 100)
  (guess-my-number))

;; **************** -*- Summary -*- ****************
;; *                   Chapter 1                   *
;; *  In this chapter, we discussed the different  *
;; * dialects of Lisp and installing CLISP. You    *
;; * learned the following along the way:          *
;; *                                               *
;; * - There are two main dialects of Lisp: Common *
;; * Lisp and Scheme. Both have a lot to offer,    *
;; * but we’ll focus on Common Lisp in this book.  *
;; *                                               *
;; * - Common Lisp is a multiparadigm language,    *
;; * meaning that it supports many different       *
;; * programming styles.                           *
;; *                                               *
;; * - CLISP is a Common Lisp implementation that  *
;; * is easy to set up, making it a great choice   *
;; * for a Lisp novice.                            *
;; *                                               *
;; * - You can type in Lisp commands directly from *
;; * the CLISP REPL.                               *
;; *************************************************
;; *                   Chapter 2                   *
;; *  In this chapter, we discussed the basic      *
;; * Common Lisp commands for defining variables   *
;; * and functions. Along the way, you learned the *
;; * following:                                    *
;; *                                               *
;; * - To define a global variable, use the        *
;; * defparameter command.                         *
;; *                                               *
;; * - To define a global function, use the defun  *
;; * command.                                      *
;; *                                               *
;; * - Use the let and flet commands to define     *
;; * local variables and functions, respectively.  *
;; *                                               *
;; * - The function labels is like flet, but it    *
;; * lets functions call themselves. Functions     *
;; * that call themselves are called recursive     *
;; * functions.                                    *
;; *************************************************
