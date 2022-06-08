# Solving a 3 by 3 Slide Puzzle

Haskell functions for solving any 3 by 3 slide puzzle.

Here, a slide puzzle is represented by a string of the digits 1..9.
Every digit represents a tile. The digit 9 plays the role of the empty tile.

Example:
```
346
758    is represented by "346758912"
 12
```

The idea of the algorithm is to build a map of all possible configurations of the tiles.
There are at most 9! = 362880 such configurations, thus the map will not be too large.

It turns out that only half of these 9! configurations are actually "possible", i.e. can be generated from the "start" configuration 123456789.

Files:
* main.hs: contains a program with a primitive user-interface. See module header for how to use it.
* boards.hs: Basic operations on a 3x3 board of tiles.
* generations.hs: Generating new boards from a given board repeatedly.
* gamemaps.hs: A map describing all possible boards and their solutions.
* other files: utilities or tests.

