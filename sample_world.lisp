(defun setup-sample-world ()

	;; Items as simple plists defroomed onto *items*
	(defitem :name "Sword" :description "A sharp steel blade." :properties "attacks")
	(defitem :name "Shield" :description "A sturdy wooden shield." :properties "None")
	(defitem :name  "Potion" :description "A healing concoction." :properties "None")
	(defitem :name "Lantern" :description "Lights up dark places." :properties "None")
	(defitem :name "Key" :description "Opens a mysterious door." :properties "None")

	;; Rooms represented as plists in *rooms* to avoid depending on CLOS 'room class
	(defroom :name "Entrance Hall" :description "The entrance to the dungeon." :special_attributes "None")
	(defroom :name "Armory" :description "Packed with old weapons." :special_attributes "None")
	(defroom :name "Library" :description "Dusty books line the walls." :special_attributes "None")
	(defroom :name "Crypt" :description "Cold and eerie." :special_attributes "None")
	(defroom :name "Treasure Room" :description "Filled with gold." :special_attributes "None")
	(defroom :name "Dungeon Cell" :description "A locked prison room." :special_attributes "None")
	(defroom :name "Hallway" :description "Long and narrow." :special_attributes "None")
	(defroom :name "Garden" :description "Overgrown with weeds." :special_attributes "None")
	(defroom :name "Forge" :description "Still warm from recent use." :special_attributes "None")
	(defroom :name "Observatory" :description "A view to the stars." :special_attributes "None")

	(format t "Sample world created: ~d items, ~d rooms.~%" (length *items*) (length *rooms*))
)

(setup-sample-world)