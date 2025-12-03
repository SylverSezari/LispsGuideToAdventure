;;;Lists
(defparameter *rooms* '())
(defparameter *items* '())
(defparameter *actions* '())
(defparameter *goal-room* nil)

;;;Classes
(defclass rooms ()
  ((name :accessor room-name)
    (description :accessor room-description)
    (items :accessor room-items :initform '())
    (attributes :accessor room-attributes :initform '())
    (exits :accessor room-exits :initform '())))

(defclass item ()
  ((name :accessor item-name)
    (description :accessor item-description)
    (properties :accessor item-properties)))

(defclass action ()
  ((name :accessor action-name)
    (parameters :accessor action-parameters :initform '()) 
    (body :accessor action-body)
    (property :accessor action-property)))


(defclass player ()
  ((location :accessor player-location)
   (inventory :initarg :inventory :accessor player-inventory :initform '())))

(defparameter *player* (make-instance 'player))

;;;Macros
(defmacro defroom (&key name description special_attributes)
  `(let ((rooms-inst (make-instance 'rooms)))
      (setf (room-name rooms-inst) ,name)
      (setf (room-description rooms-inst) ,description)
      (unless (string= ,special_attributes "None")
        (setf (room-attributes rooms-inst) ,special_attributes))
     (push rooms-inst *rooms*)
     rooms-inst))

(defmacro defitem (&key name description properties)
  `(let ((item-inst (make-instance 'item)))
      (setf (item-name item-inst) ,name)
      (setf (item-description item-inst) ,description)
      (unless (string= ,properties "None")
        (setf (item-properties item-inst) ,properties))
     (push item-inst *items*)
     item-inst))

(defmacro defaction (&key name description property)
  `(let ((action-inst (make-instance 'action)))
      (setf (action-name action-inst) ,name)
      (setf (action-body action-inst) ,description)
      (unless (string= ,property "None")
        (setf (action-property action-inst) ,property))
     (push action-inst *actions*)
     action-inst))

;;;Functions

;;Generic utility functions
(defun shuffle-list (list)
  "Return a new list that is a random permutation of LIST."
  (let* ((vec (coerce list 'vector))
         (n (length vec)))
    ;; Fisher–Yates shuffle
    (loop for i from (1- n) downto 1 do
      (rotatef (aref vec i)
               (aref vec (random (1+ i)))))
    (coerce vec 'list)))


(defun find-room-by-name (name)
  (find name *rooms* :key #'room-name :test #'string=))

(defun find-item-by-name (name)
  (find name *items* :key #'item-name :test #'string=))

(defun player-has-property-p (property)
  "Returns T if any item in the player's inventory has PROPERTY."
  (let ((inv (player-inventory *player*)))
    (dolist (it inv)
      (when (string= (item-properties it) property)
        (return t)))))

(defun can-enter-room-p (target-room)
  "Check if player is allowed to move from CURRENT to TARGET."
  (let ((required-attr (room-attributes target-room)))
    (format t "Required attribute to enter ~a: ~a~%" (room-name target-room) required-attr)
    (if (or (null required-attr)
            (string= required-attr "None"))
        t
        ;; target requires an item with this property
        (player-has-property-p required-attr))))

;;Player functions

(defun go-to-room (index)
  "Move the player through exit number INDEX (1-based), respecting locks."
  (let* ((current (player-location *player*))
         (exits (room-exits current)))
    
    (if (or (<= index 0)
            (> index (length exits)))
        (format t "There is no exit number ~a.~%" index)
        
        (let* ((target-name (nth (1- index) exits))
               (target-room (find-room-by-name target-name)))

          (if (null target-room)
              (format t "This exit leads nowhere.~%")

              ;; Check entry permissions
              (if (can-enter-room-p target-room)
                  (progn
                    (setf (player-location *player*) target-room)
                    (look-around))
                  (format t "The way is blocked. You need a special item to proceed.~%")))))))

(defun look-around ()
  "Describe the current room, its exits, and any items present."
  (let ((r (player-location *player*)))
    (format t "You take a look around you. You soon realize that you are in the room: ~a.~%~%Description: ~a~%~%" 
            (room-name r) 
            (room-description r))
    ;;exits
    (let ((exits (room-exits r)))
      (if exits
          (progn
            (format t "Exits:~%")
            (loop for e in exits
                  for i from 1
                  do (format t "  ~d) ~a~%" i e)))
          (format t "There are no exits from here.~%")))
    ;; Items
    (let ((items (room-items r)))
      (when items
        (format t "~%You see the following items:~%")
        (loop for it in items
              do (format t "  - ~a~%" it))))
    (check-goal)))

(defun take (item-name)
  "Pick up ITEM-NAME from current room and add it to player inventory."
  (let* ((room (player-location *player*))
         (items (room-items room))
         (item (find item-name items :test #'string=)))

    (if (null item)
        (format t "There is no ~a here.~%" item-name)
        
        (let ((obj (find-item-by-name item-name)))
          (push obj (player-inventory *player*))
          (setf (room-items room)
                (remove item (room-items room) :test #'string=))
          (format t "You picked up the ~a.~%" item-name)
          (format t "Description:  ~a.~%" (item-description obj))))))

(defun inventory ()
  (if (null (player-inventory *player*))
      (format t "You are carrying nothing.~%")
      (format t "You are carrying: ~{~a~^, ~}~%"
              (mapcar #'item-name (player-inventory *player*)))))

;;World initialization functions
(defun distribute-items-randomly ()
  "Places every item from *items* into a random room."
  (dolist (item *items*)
    (let ((room (nth (random (length *rooms*)) *rooms*)))
      (push (item-name item) (room-items room)))))

(defun connect-rooms (room-a room-b)
  "Create a one-way exit from room-a to room-b.
Exits are stored as STRINGS (room names)."
  (let ((name-b (room-name room-b)))
    (push name-b (room-exits room-a)))
  room-a)

(defun connect-rooms-bidirectional (room-a room-b)
  (connect-rooms room-a room-b)
  (connect-rooms room-b room-a))

(defun random-exits-for-room (roomx all-rooms)
  "Assign 1-3 random exits to ROOM. Exits are stored as strings."
  (let* ((other-rooms (remove roomx all-rooms))
         (exit-count (+ 1 (random 3))) ; 1, 2, or 3
         (candidates (shuffle-list other-rooms)))
    (dotimes (i exit-count)
      (connect-rooms roomx (nth i candidates)))))

(defun connect-linearly (rooms)
  "Connect rooms in a simple chain: A→B→C→D…"
  (loop for (a b) on rooms while b do
    (connect-rooms-bidirectional a b)))

(defun add-random-branches (rooms &key (branch-factor 1))
  "Add extra random connections to create a branching dungeon."
  (dolist (roomx rooms)
    (dotimes (i (random (1+ branch-factor)))
      (let ((target (nth (random (length rooms)) rooms)))
        (unless (eq roomx target)
          (connect-rooms-bidirectional roomx target))))))

(defun add-random-loops (rooms &key (loop-chance 0.05))
  "Occasionally add a loop between far-apart rooms."
  (when (< (random 1.0) loop-chance)
    (let* ((a (nth (random (length rooms)) rooms))
           (b (nth (random (length rooms)) rooms)))
      (unless (eq a b)
        (connect-rooms-bidirectional a b)))))

(defun generate-dungeon (&key (branch-factor 1)
                              (loop-chance 0.05))
  "Generate a roguelike dungeon using *rooms*."
  (let ((rooms (shuffle-list *rooms*)))
    ;; Clear all exits from previous runs
    (dolist (r rooms)
      (setf (room-exits r) '()))
    ;; Step 1: ensure full connectivity with a spanning tree
    (connect-linearly rooms)
    ;; Step 2: add branching paths
    (add-random-branches rooms
                         :branch-factor branch-factor)
    ;; Step 3: sometimes add loops
    (add-random-loops rooms
                      :loop-chance loop-chance)
    rooms))

(defun start-game ()
  "Initializes the dungeon, assigns start & goal rooms, places items,
   distributes them, sets player position, and prints the game objective."
  ;; 1) Ensure rooms & items exist
  (unless (and *rooms* *items*)
    (format t "You must load or create a world first!~%")
    (return-from start-game nil))

  (defroom :name "Entrance Hall" :description "The entrance to the evil Kings Castle." :special_attributes "None")
  (defroom :name "Throne Entrance" :description "You have nearly reached your Goal. Only one locked door is standing in your way. Inside an angry King awaits so it might be a good Idea to bring a weapon" :special_attributes "None")
  (defitem :name "Golden Key" :description "Golden Key with diamonds on it. Used to open a big door." :properties "locked_throne")
  (defitem :name "Excalibur" :description "A legendary sword used to topple evil Kings" :properties "attack")

  ;; 2) Generate a roguelike dungeon structure
  (generate-dungeon)

  ;; 3) Distribute items randomly into rooms
  (distribute-items-randomly)

  (defroom :name "Throne Room" :description "The evil Kings Throne, an angry King awaits you here to defend his Kingdom!" :special_attributes "locked_throne")
  (defaction :name "Strike down the evil King" :description "Use your might to defeat the evil King and claim his Throne!" :property "attack")

  ;; 4) Choose start and goal rooms
  (let* ((start-room (find-room-by-name "Entrance Hall"))
        (goal-room (find-room-by-name "Throne Room")))

    ;; Save them globally
    (defparameter *start-room* start-room)
    (defparameter *goal-room* goal-room)

    ;; 5) Place player in start room
    (setf (player-location *player*) start-room)

    (connect-throne-rooms)

    ;; 6) Print story / task
    (format t "~%==============================~%")
    (format t "      WELCOME TO THE DUNGEON~%")
    (format t "==============================~%~%")

    (format t "You awaken in ~a.~%"
            (room-name start-room))
    (format t "Your goal is to reach the legendary room: ~a.~%"
            (room-name goal-room))
    (format t "Survive the labyrinth, collect useful items,~%")
    (format t "and navigate your way to the final chamber.~%~%")

    (format t "Good luck, adventurer!~%~%")

    ;; 7) Show current room
    (look-around)))

(defun check-goal()
  "Check if the player has reached the goal room."
  (if (eq (player-location *player*) *goal-room*)
    (progn (format t "~%Congratulations! You have reached the goal room: ~a.~%" (room-name *goal-room*))
    (end-fight))
    2))

(defun end-fight()
  (format t "~%As you enter the room, the evil King glares at you with fury.~%")
  (if (player-has-property-p "attack")
      (format t "With your mighty weapon, you strike down the evil King!~%You have defeated him and claimed the throne! Congratulations, you win!~%")
      (format t "Unarmed and defenseless, you fall to the evil King's wrath.~%You have been defeated. Game over.~%")))

(defun connect-throne-rooms ()
  (let ((entrance (find-room-by-name "Throne Entrance"))
        (throne   (find-room-by-name "Throne Room")))
    
    (when (and entrance throne)
      ;; Add one locked exit
      (push (room-name throne) (room-exits entrance)))))