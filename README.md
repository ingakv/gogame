# prog2006-assignment-1 Inga Kvam

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
   - All of the functions relating to rendering and drawing something to the screen, including:
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
   - Also contains all functions that doesn't fit into the other libraries, such as:
     - Initalizing the board and world
     - Updating the world
     - Mouse coordinates and placement

The textures are in the `assets` folder:

- Background
- Board
- Black and White stones
- Stones when the mouse is hovering over a slot
  - Are the above png files at 50% opacity
- A few photoshop files are also included for when I wanted to tweak something about a certain texture as I progressed in the assignment
- The commit message syntax is also in there for easy access for myself, but I will most likely not remove it as it can be useful to keep there

The font I used is in the `ttf` folder

### Testing

The test file `Spec.hs` is located in the test folder

The test [succeeded](test/gogame-0.1.0.0-gogame-test.log)

## Requirements specification

**Feature specification**

* Visualizing a Go board
* Doing moves and updating board according to Go rules
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
  * A visual of the number of groups, as well as the degree of freedom, for each player
* Starts an empty board on startup
* Skips turn by pressing 's'
* Quits the application when pressing 'q'
* Clear the board by pressing 'c'
* Can load board states from a SGF file, however the format of this file is not up to standard, and does instead only consist of an array of 'E's, 'W's and 'B's, which stands for Empty, White and Black

**Not implemented features**

* Ko is not implemented and a infinite loop of capture and recapture is possible
* Reading from SGF file is not done the "correct" way
* Can not save a game / board to SGF file

## Non-functional requirement

* The application should be written in Haskell, and using the library SDL2 for GUI
* The application must run smoothly without any major problems.
* It should be able to read an SGF file quickly and checking whether or not it exists.
* Should also have at least 50% test coverage.
* The code should be explained throughout with comments

## Assessment specification

- I will discuss the reasonings as to why certain features are not implemented, and how I would have implemented them if I could

* Assessing whether or not the program runs optimally (lag/memory problems)
* How doing the assignment in Haskell has influenced the final product
* My process of working

## Self Assessment

**Functional Assessment**

- The main reasoning as to why not all of the features are implemented is time prioritization. If I didn't have several other assignments and projects in other courses I would most likely continue to attempt to implement these.

  - I chose to not implement KO as I feel like it would be one of the more advanced features. The way I would have done it is by storing two copies of the board before any change was made to it. So board_1 would be how the board was 1 turn ago, and board_2 would be how it was 2 rounds ago. When a stone was attempted to be placed I would then check if the board was back to how it looked in one of the two previous rounds. If it was, the stone would not be able to be placed
  - I have not been able to completely learn how to implement reading from and writing to an SGF file, because of time issues. But the implementation would be done by adjusting the already implemented code for reading from a SGF / text file
- I came upon a problem when working on loading in the textures where launching the exe file would take up a lot of memory and eventually crash. I solved this by loading in the textures and the font I used for the on screen printing, into the initialWorld, rather than loading inside of the draw functions
- I have focused on one issue or feature at a time. And have made a commit each time a fix or feature has either been fully implemented or I made major progress on it.

  - An example of this is when I decided to change the method of capture. At first I did this by going through the individual stones, and after figuring out I couldn't track the groups of stones this way, I searched through the groups. I would commit when I would get the new feature to work, before it was implemented etc.
- I have followed the commit message standard to the best of my ability
