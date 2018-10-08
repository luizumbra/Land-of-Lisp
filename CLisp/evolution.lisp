(defparameter *width* 100)
(defparameter *height* 30)
(defparameter *jungle* '(45 10 10 10))
(defparameter *plant-energy* 80)

(defparameter *plants* (make-hash-table :test #'equal))

(defun random-plant (left top width height)
  "Creates a new plant within a specified region of the world."
  (let ((pos (cons (+ left (random width)) (+ top (random height)))))
    (setf (gethash pos *plants*) t)))

(defun add-plants ()
  "Add two plants every day, one inside the jungle and another in the rest of the map."
  (apply #'random-plant *jungle*)
  (random-plant 0 0 *width* *height*))

(defstruct animal
  "The animal in the game, knowing that this is a Darwinian game of survival, so if an animal can not forage enough food, it will starve and die. So, the energy field tracks how many days of energy an animal has remaining.
The animal also has to track the direction he is facing. This is important because an animal will walk to a neighboring square in the world map each day. As you can see to animal (M):

|0|1|2|
|7|M|3|
|6|5|4|

And finnaly, has the animal genes. Each animal has exactly eight genes, consisting of positive integers. Every day, an animal will decide whether to continue facing the same direction as the day before or to turn and face a new direction. Let’s represent these genes as a table, showing each slot number and how large of a value is stored in it. "
  x y energy dir genes)

(defparameter *animals*
  "A list of animals. It's started with a single animal, he will be starting at the center of the world with initial energy of a thousand (since he has not evolved yet, he needs more energy to give him a chance of survive) and, for the genes, we use random numbers."
  (list (make-animal :x      (ash *width*  -1)
                     :y      (ash *height* -1)
                     :energy 1000
                     :dir    0
                     :genes  (loop repeat 8
                                   collecting (1+ (random 10))))))

(defun move (animal)
  "This function accepts an animal as an argument and moves it, orthogo- nally or diagonally, based on the direction grid we have described. After decidind the move direction, it is needed to decrease the amount of energy the animal possesses by one. Motion, after all, requires energy."
  (let ((dir (animal-dir animal))
        (x (animal-x animal))
        (y (animal-y animal)))
    (setf (animal-x animal) (mod (+ x
                                    (cond ((and (>= dir 2) (< dir 5)) 1)
                                          ((or (= dir 1) (= dir 5)) 0)
                                          (t -1))
                                    *width*)
                                 *width*))
    (setf (animal-y animal) (mod (+ y
                                    (cond ((and (>= dir 0) (< dir 3)) -1)
                                          ((and (>= dir 4) (< dir 7)) 1)
                                          (t 0))
                                    *height*)
                                 *height*))
    (decf (animal-energy animal))))

(defun turn (animal)
  "This function will use the animal’s genes to decide if and how much it will turn on a given day."
  (let ((x (random (apply #'+ (animal-genes animal)))))
    (labels ((angle (genes x)
		    (let ((xnu (- x (car genes))))
                      (if (< xnu 0)
			  0
			(1+ (angle (cdr genes) xnu))))))
            (setf (animal-dir animal)
		  (mod (+ (animal-dir animal) (angle (animal-genes animal) x)) 8)))))

(defun eat (animal)
  "Eating is a simple process. We just need to check if there’s a plant at the ani- mal’s current location, and if there is, consume it.
The animal’s energy is increased by the amount of energy that was being stored by the plant. We then remove the plant from the world using the remhash function."
  (let ((pos (cons (animal-x animal) (animal-y animal))))
    (when (gethash pos *plants*)
      (incf (animal-energy animal) *plant-energy*)
      (remhash pos *plants*))))

(defparameter *reproduction-energy* 200)

(defun reproduce (animal)
  "The animal will be a asexually reproducer. It takes a healthy parent to produce healthy offspring, so our animals will reproduce only if they have at least the value of *reproduction-energy* days’ worth of energy.
To create the new animal, we simply copy the structure of the parent."
  (let ((e (animal-energy animal)))
    (when (>= e *reproduction-energy*)
      (setf (animal-energy animal) (ash e -1))
      (let ((animal-nu (copy-structure animal))
            (genes     (copy-list (animal-genes animal)))
            (mutation  (random 8)))
        (setf (nth mutation genes) (max 1 (+ (nth mutation genes) (random 3) -1)))
        (setf (animal-genes animal-nu) genes)
        (push animal-nu *animals*)))))

(defun update-world ()
  "This function removes all dead animals from the world."
  (setf *animals* (remove-if (lambda (animal)
                               (<= (animal-energy animal) 0))
                             *animals*))
  (mapc (lambda (animal)
          (turn animal)
          (move animal)
          (eat animal)
          (reproduce animal))
        *animals*)
  (add-plants))

(defun draw-world ()
  "A simulated world isn’t any fun unless we can actually see our critters running around, searching for food, reproducing, and dying. The draw-world function handles this by using the *animals* and *plants* data structures to draw a snap- shot of the current world."
  (loop for y
        below *height*
        do (progn (fresh-line)
                  (princ "|")
                  (loop for x
                        below *width*
                        do (princ (cond ((some (lambda (animal)
                                                 (and (= (animal-x animal) x)
                                                      (= (animal-y animal) y)))
                                               *animals*)
                                         #\M)
                                        ((gethash (cons x y) *plants*) #\*)
                                        (t #\space))))
                  (princ "|"))))

(defun evolution ()
  "A user interface function for our simulation. If the user enters a valid integer n, the program will run the simulation for n simulated days, using a loop. If the input isn’t a valid integer, we run update-world to simulate one more day."
  (draw-world)
  (fresh-line)
  (let ((str (read-line)))
    (cond ((equal str "quit") ())
          (t (let ((x (parse-integer str :junk-allowed t)))
               (if x
                   (loop for i
			 below x
			 do (update-world)
			 if (zerop (mod i 1000))
			 do (princ #\.))
                 (update-world))
               (evolution))))))
