;; "Orc Battle"
;;  ===========
;;
;; Description:
;; ===========
;;   Program of the book "Land of Lisp". In the Orc Battle game, you’re a knight
;; surrounded by 12 monsters, engaged in a fight to the death. With your superior wits
;; and your repertoire of sword- fighting maneuvers, you must carefully strategize in
;; your battle with orcs, hydras, and other nasty enemies. One wrong move and you may
;; be unable to kill them all before being worn down by their superior numbers.
;;
;; Copyright
;; =========
;;   Author of the book "Land of Lisp"

;; Player stats: health, agility and strength
(defparameter *player-health* nil
  "When *player-health* reaches zero, that player will die.")
(defparameter *player-agility* nil
  "Control how many attacks a player can perform in a single round of battle.")
(defparameter *player-strength* nil
  "Control the ferocity of the attacks.")

;; Building monsters
(defparameter *monsters* nil
  "Heteroggeneous array, it can contain different types of monsters.")
(defparameter *monster-builders* nil
  "List of functions for building monsters.")
(defparameter *monster-num* 12
  "Control how many oppo- nents our knight must fight. Change this variable to increase (or decrease) the difficulty level.")

;; Main Game Function
(defun orc-battle ()
  "This function will initialize the monsters and start the game loop and, once the battle ends, it will deter- mine the victor and print the appropriate ending message for the game."
  (init-monsters)
  (init-player)
  (game-loop)
  (when (player-dead)
    (princ "You have been killed. Game Over."))
  (when (monsters-dead)
    (princ "Congratulations! You have vanquished all of your foes.")))

(defun game-loop ()
  "This function handles a round of the battle, and then calls itself recursively for the following round."
  (unless (or (player-dead) (monsters-dead))
    (show-player)
    (dotimes (k (1+ (truncate (/ (max 0 *player-agility*) 15))))
      (unless (monsters-dead)
        (show-monsters)
        (player-attack)))
    (fresh-line)
    (map 'list
         (lambda(m)
           (or (monster-dead m) (monster-attack m)))
         *monsters*)
    (game-loop)))

(defun init-player ()
  "This function initialize the player's sttribute."
  (setf *player-health* 30)
  (setf *player-agility* 30)
  (setf *player-strength* 30))

(defun player-dead ()
  "This function check if the player is alive. For that, this will check if *player-health* is bigger than zero.*"
  (<= *player-health* 0))

(defun show-player ()
  "Show the player's status."
  (fresh-line)
  (princ "You are a valiant knight with a health of ")
  (princ *player-health*)
  (princ ", an agility of ")
  (princ *player-agility*)
  (princ ", and a strength of ")
  (princ *player-strength*))

(defun player-attack ()
  "This function prints out some different types of attacks from which the player
  can choose.

  Stab (s) permit the player to choose a monster and attack him with just one attack, with a random factor that generate a nice, ut never too powerful, attack strength.

  Double Swing (d) a weak attack (with strength already displayed) that allows two enemies, of the player choose, to attacked at once.

  Roundhouse (r) this final attack is a wild, chaotic attack that does not discriminate among the enemies. Each attack is very weak."
  (fresh-line)
  (princ "Attack style: [s]tab [d]ouble swing [r]oundhouse:")
  (case (read)
    (s (monster-hit (pick-monster)
                    (+ 2 (randval (ash *player-strength* -1)))))
    (d (let ((x (randval (truncate (/ *player-strength* 6)))))
         (princ "Your double swing has a strength of ")
         (princ x)
         (fresh-line)
         (monster-hit (pick-monster) x)
         (unless (monsters-dead)
           (monster-hit (pick-monster) x))))
    (otherwise (dotimes (x (1+ (randval (truncate (/ *player-strength* 3)))))
                 (unless (monsters-dead)
                   (monster-hit (random-monster) 1))))))

(defun randval (n)
  "A helper function to generate some randomness to the attacks in the player-attack function. This function garantees that the player attack will be at least one."
  (1+ (random (max 1 n))))

(defun random-monster ()
  "A helper function that picks a monster to target the chaotic roundhouse attack, while ensuring that the chosen monster is not already dead."
  (let ((m (aref *monsters* (random (length *monsters*)))))
    (if (monster-dead m)
        (random-monster)
      m)))

(defun pick-monster ()
  "A helper function that allows the player to pick a monster to target for the nonrandom attacks. The monster choosen need to be in the *monsters* array and not be dead."
  (fresh-line)
  (princ "Monster #:")
  (let ((x (read)))
    (if (not (and (integerp x) (>= x 1) (<= x *monster-num*)))
        (progn (princ "That is not a valid monster number.")
               (pick-monster))
      (let ((m (aref *monsters* (1- x))))
        (if (monster-dead m)
            (progn (princ "That monster is alread dead.")
                   (pick-monster))
          m)))))

(defun init-monsters ()
  "This function initialize all the bad guys stored in the *monsters* array. To that, it will choose a random constructor function of the *monster-builders* lists."
  (setf *monsters*
        (map 'vector
             (lambda (x)
               (funcall (nth (random (length *monster-builders*))
                             *monster-builders*)))
             (make-array *monster-num*))))

(defun monster-dead (m)
  "Check if a monster m is dead."
  (<= (monster-health m) 0))

(defun monsters-dead ()
  "Check if all monsters is dead."
  (every #'monster-dead *monsters*))

(defun show-monsters ()
  "Function to display a listing of all the monsters."
  (fresh-line)
  (princ "Your foes:")
  (let ((x 0))
    (map 'list
         (lambda (m)
           (fresh-line)
           (princ "   ")
           (princ (incf x))
           (princ ". ")
           (if (monster-dead m)
               (princ "**dead**")
             (progn (princ "(Health=")
                    (princ (monster-health m))
                    (princ ") ")
                    (monster-show m))))
         *monsters*)))

;; Create life to the monsters

(defstruct monster
  "Generic monster. All monsters must have at least a health parameter bigger than 0."
  (health (randval 10)))

(defmethod monster-hit (m x)
  "A method of monster struct that takes away a monster’s (m) health by x when it’s attacked. It also output a message explaining what happened, including a message of the death of a monster."
  (decf (monster-health m) x)
  (if (monster-dead m)
      (progn (princ "You killed the ")
             (princ (type-of m))
             (princ "! "))
    (progn (princ "You hit the ")
           (princ (type-of m))
           (princ ", knocking off ")
           (princ x)
           (princ " health points! "))))

(defmethod monster-show (m)
  "Show the monster type."
  (princ "A fierce ")
  (princ (type-of m)))

(defmethod monster-attack (m))

;; Ors structure

(defstruct (orc (:include monster))
  "A monster of type orc. He can deliver a strong attack with his club, but other- wise he is pretty harmless. Every orc has a club with a unique attack level. Orcs are best ignored, unless there are orcs with an unusually powerful club attack that you want to cull from the herd at the beginning of a battle."
  (club-level (randval 8)))

(push #'make-orc *monster-builders*)

(defmethod monster-show ((m orc))
  "Show to the player the orc level club."
  (princ "A wicked orc with a level ")
  (princ (orc-club-level m))
  (princ " club"))

(defmethod monster-attack ((m orc))
  "Use the level of the club to decide how badly the player is hit by the club. Describing it to the player."
  (let ((x (randval (orc-club-level m))))
    (princ "An orc swings his club at you and knocks off ")
    (princ x)
    (princ " of your health points. ")
    (decf *player-health* x)))

;; Hydra structure

(defstruct (hydra (:include monster))
  "Hydra is a monster type. She is a very nasty enemy. It will attack you with its many heads, which you’ll need to chop off to defeat it. The hydra’s special power is that it can grow a new head during each round of battle, which means you want to defeat it as early as possible.")

(push #'make-hydra *monster-builders*)

(defmethod monster-show ((m hydra))
  "Show to the player the number of hydra's heads. The number of heads it is also hers health atribute."
  (princ "A malicious hydra with ")
  (princ (monster-health m))
  (princ " heads."))

(defmethod monster-hit ((m hydra) x)
  "When an Hydra is hit, she loses a head to every blow, showing the number of heads decapted by the player. Remeber, the number of hydra's head is the same as her health."
  (decf (monster-health m) x)
  (if (monster-dead m)
      (princ "The corpse of the fully decapitated and decapacitated hydra
falls to the floor!")
    (progn (princ "You lop off ")
           (princ x)
           (princ " of the hydra's heads! "))))

(defmethod monster-attack ((m hydra))
  "Use a random function to attak with the hydra's head, its also increase a head to the hydra."
  (let ((x (randval (ash (monster-health m) -1))))
    (princ "A hydra attacks you with ")
    (princ x)
    (princ " of its heads! It also grows back one more head! ")
    (incf (monster-health m))
    (decf *player-health* x)))

;; Slime-mold structere

(defstruct (slime-mold (:include monster))
  "Slime mold is a unique monster. When it attacks you, it will wrap itself around your legs and immobilize you, letting the other bad guys finish you off. It can also squirt goo in your face, decreasing the player agility."
  (sliminess (randval 5)))

(push #'make-slime-mold *monster-builders*)

(defmethod monster-show ((m slime-mold))
  "Show to the player the slime and his sliminess capability."
  (princ "A slime mold with a sliminess of ")
  (princ (slime-mold-sliminess m)))

(defmethod monster-attack ((m slime-mold))
  "The attack of the slime must do some special things, which allow it to immobilize the player. First, it uses the slime mold’s sliminess (which is generated when each slime mold is built) to generate a random attack against the player. And unlike most other attacks in the game, this slime mold attack affects the agility of players, rather than their healt. Therefore, the slime mold also has a superwimpy squirt attack that happens during half of all attacks, but sub- tracts only a single health point from the player."
  (let ((x (randval (slime-mold-sliminess m))))
    (princ "A slime mold wraps around your legs and decreases your agility
by ")
    (princ x)
    (princ "! ")
    (decf *player-agility* x)
    (when (zerop (random 2))
      (princ "It also squirts in your face, taking away a health point! ")
      (decf *player-health*))))

;; Brigand structure

(defstruct (brigand (:include monster))
  "The brigand is the smartest of all your foes. He can use his whip or slingshot and will try to neutralize your best assets. His attacks are not powerful, but they are a consistent two points for every round.")

(push #'make-brigand *monster-builders*)

(defmethod monster-attack ((m brigand))
  "The brigand attack must choose the biggest player atributes (health, agility and strength) to focus his attack. If several of the attributes are equally large, the brigand will choose health over agility and agility over strength as the focus of attack.

  If health is the largest value, the player is hit with a slingshot.

  If agility is the largest, the brigand will whip the player’s leg.

  If strength is the largest, the brigand will whip the player’s arm.

  His attack always drop in 2 points the player attribute."
  (let ((x (max *player-health* *player-agility* *player-strength*)))
    (cond ((= x *player-health*)
           (princ "A brigand hits you with his slingshot, taking off 2 health points! ")
           (decf *player-health* 2))
          ((= x *player-agility*)
           (princ "A brigand catches your leg with his whip, taking off 2
agility points! ")
           (decf *player-agility* 2))
          ((= x *player-strength*)
           (princ "A brigand cuts your arm with his whip, taking off 2 strength points! ")
           (decf *player-strength* 2)))))
