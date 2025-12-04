# Lisp's Guide to Adventure

Lisp's Guide to Adventure gives you presets to create Rooms, Items and Actions to create your own little Adventure. It will then create a random dungeon for you to explore.
A sample world is included in `sample_world.lisp` that you can load to see how it works.


## How to create your own objects

### Rooms
`(defroom :name "room-name" :description "room-description" :requirements "room-requirements")`

### Items
`(defitem :name "item-name" :description "item-description" :properties "item-properties")`

### Actions
`(defaction :name "action-name" :description "action-description" :requirements "action-requirements" :properties "action-properties")`

### Logic to requirements and properties
- The properties of an item and requirements of an action are strings that need to be matched in order to perform the action with the item.
- The properties of an action and the requirements of a room are strings that need to be matched in order to unlock the room with the action.


## Setup and Running Instructions

### Setup
To install Steel Bank Common Lisp, which I use, go to it's website: [sbcl](http://www.sbcl.org/)

### Running
To run the game, load the `gameEngine.lisp` file in your Common Lisp environment and either type in all the functions to add rooms, items and actions seperately or put them in a file and load it aswell.</br>
To load a file in Common Lisp, go to the file directory where `gameEngine.lisp` is located and open the console. Then type:
```cl
sbcl --load *file_name*.lisp
```
or if you have already started lisp, type `(load "filename".lisp)`. Then call the function `(start-game)`. Follow the on-screen prompts to play the game.</br>
To restart the game, close sbcl by typing `(quit)` and then do the starting process again.

### Available commands during gameplay
- `(start-game)` : Starts the game.
- `(look-around)` : Describes your current location.
- `(go-to-room room-index)` : Move to a different room by specifying its index.
- `(take "item-name")` : Pick up an item in the current room by specifying its name.
- `(inventory)` : Lists all items in your inventory.
- `(show-actions)` : Lists all available actions and their requirements.
- `(do-action "action-name" room-index)` : Perform an action by specifying its name and the room you want to use it on.