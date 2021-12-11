# brick-simple-sokoban
Final Project for UCSD CSE 230 Fall 2021

## Group Member
NAME: ZIZHOU WANG   PID: A59002622 (alone)

## Game
Sokoban is a classic video game in which the player pushes crates or boxes around in a warehouse, trying to get them to storage locations (called holes in Chinese).

This project is an implementation of Sokoban game in Haskell using the brick library.
The application will render the game in command line using the vty GUI library.

### Rules

The capability of the man in the Sokoban world is limited:
1. Moving will be blocked on the wall brick.
2. The man can only push one box at a time. If there is a wall brick or another box behind, current box then cannot be pushed. 
3. When pushed to a hole, a box can still be pushed away.

The game will succeed if all boxes are pushed to holes. However, sometimes a box will be stuck in a situation where it cannot be moved anymore. In such case, the player could actually never finish the game.

### Operations
The player could use 'w' 'a' 's' 'd' to move the man in Sokoban.
While selecting stages, use 'j' / 'k' to select previous / next one.
Use 'q' to give up the game when the game is running.
In other cases, 'q' is used to go back in page or quit the game.
Use 'Enter' to enter selecting page / start / restart the game.

## Build & Execute
```bash
$ stack build
$ stack brick-simple-sokoban-exe
```