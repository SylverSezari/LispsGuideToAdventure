(defun setup-sample-world ()

	;; Items as simple plists defroomed onto *items*
	(defitem :name "Sword" :description "A sharp steel blade." :properties "None")
	(defitem :name "Shield" :description "A sturdy wooden shield." :properties "None")
	(defitem :name  "Potion" :description "A healing concoction." :properties "None")
	(defitem :name "Lantern" :description "Lights up dark places." :properties "light source")
	(defitem :name "Key" :description "Opens a mysterious door." :properties "None")

	;; Rooms represented as plists in *rooms* to avoid depending on CLOS 'room class
	(defroom :name "Entrance Hall" :description "The entrance to the dungeon." :requirements "None")
	(defroom :name "Armory" :description "Packed with old weapons." :requirements "None")
	(defroom :name "Library" :description "Dusty books line the walls." :requirements "None")
	(defroom :name "Crypt" :description "Cold and eerie. Only your lightsource keeps the dark away" :requirements "light")
	(defroom :name "Treasure Room" :description "Filled with gold." :requirements "None")
	(defroom :name "Dungeon Cell" :description "A locked prison room." :requirements "None")
	(defroom :name "Hallway" :description "Long and narrow." :requirements "None")
	(defroom :name "Garden" :description "Overgrown with weeds." :requirements "None")
	(defroom :name "Forge" :description "Still warm from recent use." :requirements "None")
	(defroom :name "Observatory" :description "A view to the stars." :requirements "None")

	;;Actions
	(defaction :name "light lantern" :description "Lights up dark areas." :requirements "light source" :property "light")

	(format t "Sample world created: ~d items, ~d rooms ~d actions.~%" (length *items*) (length *rooms*) (length *actions*))
)

(setup-sample-world)