# prog2006-assignment-1 Inga Kvam



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
* When hovering over an available intersection, a semi-transparent marker with the correct color will be shown.
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

* 

* I will look at the methods of capture (if any), as well as the complexity of the functions (are they more complicated than neccassary?)

* What features are implemented, and to what degree.

* Assessing whether or not the program runs optimally (lag/memory problems)

* Checking for glitches/crashes, how preventable?

  



## Self Assessment

**Functional Assesment**

A self-assessment following the above assessment specification. (as a report: **Self-Assessment**)




**Reasoning for non-implementation**

* Main reason is time constraints













## Important
* Make the merge request against THIS repo. Do not add your own project on top of the existing project, instead, extend the project by the structure that already exists and modify the required files.









# Assignments

## General notes

* Professionalism is expected, and this includes:
  * Documentation, comments, explanations, installation instructions, and so on
  * Code Quality
  * Test and Lint coverage
  * Complexity analysis where appropriate
  * Explanations related to the choice of algorithms, modularity, maintainability
  * No rounding errors or explicit control of rounding errors when appropriate (eg. no rounding errors are acceptable for financial operations)
* 
* Code must be in the Git repo and linked with semantic commits (linked to issues).
* 
* 
* A video with [show-and-tell](https://en.wikipedia.org/wiki/Show_and_tell) presentation of the assignment demo, code, assessment, and reflections. The video link must be added to the Self-Assessment report.

