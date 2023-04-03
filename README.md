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



```


 **Not implemented features**

* Self-capture is not checked and is allowed
* Ko is not checked and a infinite loop of capture and recapture is possible
* Can NOT read from already generated games, have to manually input each move


```



## Non-functional requirement

* The application should be written in Haskell, and using the library SDL2 for GUI
* The application must run smoothly without any major problems.
* It should be able to read an SGF file quickly and checking whether or not it exists.
* Should also have at least 50% test coverage.
* The code should be explained throughout with comments



## Assessment specification

* ```
  * An assessment specification for validation and verification of the assignment, taking into account all of the above points. (as a report: **Assessment Specification**)
  * 
  * I will look at the methods of capture (if any), as well as the complexity of the functions (are they more complicated than neccassary?)
  * What features are implemented, and to what degree.
  * Assessing whether or not the program runs optimally (lag/memory problems)
  * Checking for glitches/crashes, how preventable?
  ```

  



A self-assessment following the above assessment specification. (as a report: **Self-Assessment**)

## Self Assessment

**Functional Assesment**




**Reasoning for non-implementation**

* Main reason is time constraints





## Assignment 1

* Submission Deadline: **April 6, 23:59**
* Submission must be done through [the submission system](http://10.212.175.82)
   - http://10.212.175.82
   - Hash: `cg49jme8eh9dbc0db0sg`

## Important
* For generic professionalism requirements refer to [the Course Wiki page](https://git.gvk.idi.ntnu.no/course/prog2006/prog2006-2023/-/wikis/home#assignments)
* Initial [specification discussion has been recorded in the class as video](https://youtu.be/PTospJzUtF0).
* Make the merge request against THIS repo. Do not add your own project on top of the existing project, instead, extend the project by the structure that already exists and modify the required files.

## Notes

* **Bonus for `>C`** Write a functions that returns `True` if two board states are isomorphic. Use translation, rotation, and symmetry operations for isomorphism tests. We assume a stones and groups must maintain the exact freedom degree counts. Or, in other words, given two SGF files with a board state only, the program checks if the two games are the same (isomorphic for translation, rotation and symmetry).







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

