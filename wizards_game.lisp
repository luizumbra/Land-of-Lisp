;;;; -*- Wizards Game -*-
;;; @author: Conrad Barsli, M. D.
;;; @book: Land of Lisp: Learn to program in Lisp, one game at a time!
;;; @date: 2011
;;;; -*- Copyright C 2011 by Conrad Barski, M.D. -*-

(defparameter *nodes*
  '((living-room (you are in the living-room.
		      a wizard is snoring loudly on the couch.))
    (garden (you are in a beautiful garden.
		 there is a well in front of you.))
    (attic (you are in the attic.
		there is a giant welding torch in the corner.)))
    "Alist representing the nodes of the world with a simple description. Each node has the following structure: (location description).")

(defun describe-location (location nodes)
  "A simple function to show the surrounding of a determined location. It will use location as a key to find the description inside node.

  Args:
    location (Symbol): symbol that represent one location of the world.
    nodes (Alist): alist representing the world graph. Its use *nodes* as default.

  Return:
    (Symbol): Return the description of location inside nodes."
  (cadr (assoc location nodes)))

(defparameter *edges*
  '((living-room (garden west door)  
                 (attic upstairs ladder))
    (garden (living-room east door))
    (attic (living-room downstairs ladder)))
  "List representing the world edges graph. Each edge has the following structure: ((location-key (location-connection direction access-to-location-conection) ...) ...)")

(defun describe-path (edge)
  "Using an edge from *edges* it describe to the user where and how to go to a conect location.

  Args:
    edge (List): an edge of *edges* with the following structure: (location-conection direction access-to-location-conection).

  Return:
    (List): Return a simple description of how to go to a determined location."
  `(there is a ,(caddr edge) going ,(cadr edge) from here.))

(defun describe-paths (location edges)
  "Describe all the paths of current location.

  Args:
    location (Symbol): symbol that represent the player current location.
    edges (List): list representing the connections between location. Its uses *edges* as default.

  Return:
    (List): Return a simple description of possible paths from the current location using describe-paths."
  (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))

(defparameter *objects*
  '(whiskey bucket frog chain)
    "List with the symbols that represent the collectable objects.")

(defparameter *object-locations*
  '((whiskey living-room)
    (bucket living-room)
    (chain garden)
    (frog garden))
    "List of Alists with the respective locations of collectable objects. It has the following structure: ((object node)...)")

(defun objects-at (loc objs obj-loc)
  "Return the objects inside a determined location.

  Args:
    loc (Symbol): a symbol representing a location (node) inside the game.
    objs (List): list of game objects. Its uses *objects* as default.
    obj-loc (List): a list of alist represent where a determined object is located. Its use *object-locations*.

  Return:
    (List): Return a simple list with objs conected to loc."
  (labels ((is-at (obj)
		  (eq (cadr (assoc obj obj-loc)) loc)))
	  (remove-if-not #'is-at objs)))

(defun describe-objects (loc objs obj-loc)
  "Decribe a object in the determined location game.

  Args:
    loc (Dymbol): a symbol representing a location (node) inside the game.
    objs (List): list of game objects. Its uses *objects* as default.
    obj-loc (List): a list of alist represent where a determined object is located. Its use *object-locations*.

  Return:
    (List): Return a list describing what objects the player can see on the floor (loc/node)."
  (labels ((describe-obj (obj)
			 `(you see a ,obj on the floor.)))
	  (apply #'append (mapcar #'describe-obj (objects-at loc objs obj-loc)))))

(defparameter *location*
  "Symbol representing the current location of the player. It is initialized with living-room"
  'living-room)

(defun look ()
  "A simple engine (not functional) to describe the node around the player.

  Args:

  Return:
    (List<Symbols>): Append in a list the describes functions of the current location of the player, represented by the global variable *location*."
  (append (describe-location *location* *nodes*)
          (describe-paths *location* *edges*)
          (describe-objects *location* *objects* *object-locations*)))

(defun walk (direction)
  "Function to determine what direction the player want to go.

  Args:
    direction (Symbol): represent a valid destination inside the game world. It must be conect with the current-location, *location*, of the player.

  Return:
    Case valid direction, this function set the global variable *location* and call the look function. Otherwise, return a error message."
  (labels ((correct-way (edge)
			(eq (cadr edge) direction)))
	  (let ((next (find-if #'correct-way (cdr (assoc *location* *edges*)))))
	    (if next 
		(progn (setf *location* (car next)) 
                       (look))
              '(you cannot go that way.)))))

(defun pickup (object)
  "Function to permit the player pickup a determined object, if this object is in the current location.

  Args:
    object (Symbol): a valid object inside *objects* list.

  Return:
    Case valid object, push the object in *object-location* together with body symbol to represent as a picked object and launch a success message. Otherwise, launch a error message."
  (cond ((member object (objects-at *location* *objects* *object-locations*))
         (push (list object 'body) *object-locations*)
         `(you are now carrying the ,object))
	(t '(you cannot get that.))))

(defun inventory ()
  "Show all objected picked by the player.

  Return:
    (List): A list with all objects in *object-locations* marked with body symbol."
  (cons 'items- (objects-at 'body *objects* *object-locations*)))

(defun have (object)
  "Inform the player if he has the selected object.

  Args:
    object (Symbol): a symbol representing a valid object.

  Return:
    t, case object is member of inventory() function. nil, otherwise."
  (member object (cdr (inventory))))

(defun game-repl ()
  "A simple game repel to the player.

  Return:
    Symbol: a quit symbol case the player exit the game. Otherwise call some command given by the player."
  (let ((cmd (game-read)))
    (unless (eq (car cmd) 'quit)
      (game-print (game-eval cmd))
      (game-repl))))

(defun game-read ()
  "Read a command given by the player.

  Return:
    (Function): a function that represents a player command."
  (let ((cmd (read-from-string (concatenate 'string "(" (read-line) ")"))))
    (flet ((quote-it (x)
                     (list 'quote x)))
          (cons (car cmd) (mapcar #'quote-it (cdr cmd))))))

(defparameter *allowed-commands* '(look walk pickup inventory)
  "A list representing the allowed commands wich the player can give.")

(defun game-eval (sexp)
  "Evaluate a command given by the player.

  Args:
    (Function): a command writen by the player.

  Return:
    (Eval Function): case the sexp is a valid command.
    (List): a list explain command invalid, case sexp is not a valid command."
  (if (member (car sexp) *allowed-commands*)
      (eval sexp)
    '(i do not know that command.)))

(defun tweak-text (lst caps lit)
  "Looks at each character in the list and modifies it as needed.

  Args:
    lst (List): A message wich repel will pass to the player.
    caps (Boolean): Define if this character must be captalized.
    lit (Boolean): Define if this character quotation mark.

  Return:
    (String): Fuse lst symbols as a string message to the player."
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
  "Print the repel message to the player.

  Args:
    lst (List): a message repel in form of list.

  Return:
    (String): Convert lst in a message readble for the player."
  (princ (coerce (tweak-text (coerce (string-trim "() " (prin1-to-string lst)) 'list) t nil) 'string))
  (fresh-line))

;; **************** -*- Summary -*- ****************
;; *                   Chapter 3                   *
;; * - Parentheses in Lisp are there to keep the   *
;; * amount of syntax to a minimum.                *
;; * - Lists are created from cons cells.          *
;; * - You can create lists by making cons cells   *
;; * with the cons command.                        *
;; * - You can inspect the pieces of a list with   *
;; * car and cdr.                                  *
;; *************************************************
;; *                   Chapter 4                   *
;; * - The values nil, 'nil, (), and '() are all   *
;; * basically the same thing in Common Lisp.      *
;; * - Lisp makes it easy to check for empty lists.*
;; * This makes it simple to write list-eaters.    *
;; * - Lisp conditionals, such as the if command,  *
;; * cause Lisp code to be evaluated only under    *
;; * the right conditions.                         *
;; * - If you need a conditional command that does *
;; * everything, then you want to use cond.        *
;; * - Comparing stuff in Lisp is complicated, but *
;; * you can get by if you just use eq for         *
;; * comparing symbols and equal for comparing     *
;; * everything else.                              *
;; *************************************************
;; *                   Chapter 5                   *
;; * - A game world can be represented by a        *
;; * mathematical graph, consisting of nodes for   *
;; * the places the player can visit and edges for *
;; * the paths between these places.               *
;; * - You can store these nodes in an association *
;; * list (alist) called *nodes*. This alist allows*
;; * you to look up properties of a node/place by  *
;; * using its name. In the case of our game, the  *
;; * property we’re storing is a description of    *
;; * each node/place.                              *
;; * - You use the assoc function to look up a key *
;; (location name in our example) in an alist.     *
;; * - Quasiquoting is a technique that allows you *
;; * to insert small bits of computer code into    *
;; * larger pieces of data.                        *
;; * - Some Lisp functions accept other functions  *
;; * as arguments. These are called higher-order   *
;; * functions. The mapcar function is the most    *
;; * popular higher-order function in Common Lisp. *
;; * - To replace a value from an alist, you push  *
;; * new items onto the list. Only the most recent *
;; * value will be reported by the assoc function. *
;; *************************************************
;; *                   Chapter 6                   *
;; * - The print and read functions let you        *
;; * directly communicate with the user through the*
;; * console. These two functions work in a        *
;; * computer-friendly way.                        *
;; * - Other input/output functions are not as     *
;; * elegant as read and print, but are friendlier *
;; * for interacting with humans. Examples include *
;; * princ and read-line.                          *
;; * - A homoiconic programming language stores its*
;; * program code and program data in a similar    *
;; * format. Lisp’s quoting, quasiquoting, eval,   *
;; * and macro features make it extremely          *
;; * homoiconic.                                   *
;; * - It’s easy to write your own custom REPL.    *
;; * - It’s simple to transform your internal Lisp *
;; * data into the format most suitable for your   *
;; * program’s interface. This makes it easy to    *
;; * separate presentation details from your       *
;; * program’s internal data structures.           *
;; *************************************************
