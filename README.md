# Tetris
This repository is based off of the code base found here: https://github.com/mgeorgoulopoulos/TetrisHaskellWeekend. 

We extended the implementation by adding the following features: 

* Calculate and display the number of lines cleared
* Calculate and display the current level, where higher levels means that the pieces fall quicker
* Added a next piece feature that displays the piece that will fall next
* Added a hold functionality, where users can hold a piece to use later on or swap the current piece with the piece on hold
* Created a scoreboard that displays the top five scores when the game is over
* Modified the code so that users can restart the game by pressing the space bar

# Compilation
Install the Gloss library if not already installed. 

Use ghc --make Main to compile the game and then type ./Main to run the executable. 

# How to Play

* Left and right arrow keys: move the falling piece left and right
* Down arrow key: hold it to accelerate the speed of the falling piece
* 'A' Key: rotates the piece clockwise
* 'D' Key: rotates the piece counterclockwise
* 'W' Key: holds a piece
* Spacebar: restarts the game
