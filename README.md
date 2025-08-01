# prog2006-assignment-1 Inga Kvam

This is a simple module for visualizing a go game, which simulate gameplay with all of the features that are included in a regular game of go. It is written in Haskell.

## Project structure

My sdl2 setup is located [here](https://github.com/ingakv/sdlpain)

The code is run with `cabal run gogame-exe`

The main file (Main.hs) is located in the `app` folder

The libraries are in the `src` folder:

1. Common.hs
   - Copied from example code by Mariusz
   - Is used for SDL2 to properly work
2. DataTypes.hs
   - Contains all the datatypes:
     - Intent
     - World
     - Slot (Empty / White / Black)
3. Draw.hs
   - All the functions relating to rendering and drawing something to the screen, including:
     - Text
     - The world
     - UI
     - The board itself
     - The stones
     - The background
   - Also contains the mainApp function
4. GameLogic.hs
   - Contains everything related to the game mechanics, including:
     - Deleting, inserting and joining together stones and groups
     - Checking free slots
     - Checking the stones that are adjacent to a given stone
   - Also contains a few smaller functions I made that are used throughout the project, such as `replace` and `insertAt`
5. Lib.hs
   - Contains all functions relating to intent (motion, button, payload, apply)
   - Contains all functions that don't fit into the other libraries, such as:
     - Initializing the board and world
     - Updating the world
     - Mouse coordinates and placement

The textures are in the `assets` folder:

- Background
- Board
- Black and White stones
- Stones when the mouse is hovering over a slot
  - Are the above png files at 50% opacity
- A few PhotoShop files are also included for when I wanted to tweak something about a certain texture as I progressed in the assignment
- The commit message syntax is also in there for easy access for myself, but I will most likely not remove it as it can be useful to keep there

The font I used is in the `ttf` folder

### Testing

The test file `Spec.hs` is located in the test folder

The test [succeeded](test/gogame-0.1.0.0-gogame-test.log)

## Requirements specification

**Feature specification**

* Visualizing a Go board
* Doing moves and updating the board according to Go rules
* Loading board state from SGF files.
* Representing groups on the board
* Counting groups and degrees of freedom
* Updating the board state via computer-generated moves
* Ability to detect illegal game moves
* Ability to properly capture stones

**Implemented features**

* The program automatically captures (groups of) stones when surrounded
* Which player / color that is active alternates each time a stone is placed
* A stone cannot be placed on top of another stone
* A stone can only be placed within the board
* Self-capture / suicide is not possible
* When hovering over an available intersection, a semi-transparent stone with the correct color will be shown.
* UI using SDL2, including

  * Textures

    * The board
    * The background
    * The stones
  * Lines
  * Numbers and letters to easily identify each intersection
  * A visual describing the number of groups, as well as the degree of freedom, for each player
* Start an empty board on startup
* Skips turn by pressing 's'
* Quit the application when pressing 'q' or the ESC button
* Clear the board by pressing 'c'
* Can load board states from an SGF file, however, the format of this file is not up to standard, and does instead only consist of an array of 'E's, 'W's and 'B's, which stands for Empty, White and Black
* Ko is implemented, which ensures that an infinite loop of capture and recapture is not possible



Should also have at least 50% test coverage.
