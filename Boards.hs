{- |
Module : Boards
Description : Basic operations on a 3x3 board of tiles.
Copyright : (c) 2022 Bruno M.S. Lustenberger

The tiles on the board are marked with the digits 1..8.
Digit 9 plays the role of the empty tile.

A state of the game is represented by a list of 9 tiles.
The first 3 tiles represent the top row, 
the next 3 tiles the middle row, the last 3 the bottom row.
The list has always length 9 and contains a permutation of 1..9

The tile 9 can be swapped with the upper, lower, left or right neighbour,
if present.

The goal of the game is to achieve the "end state" 123/456/789.
-}
module Boards
{- Uncomment for tests
    ( Tile
    , Board
    , endBoard
    , Direction (..)
    , possibleDirections
    , move
    , moves
    )
     -}
where

import Data.List (elemIndex, foldl')
import Data.Set (fromList, size)
import Data.Char (ord)

import GeneralUtils (setListElem, swapListElems)

{- 1. Operations for the algorithm -}

-- |The tiles
type Tile = Int -- only 1..9

-- |The boards
type Board = [Tile]

-- |Checks that the list satisfies the described restrictions.
isBoard :: Board -> Bool
isBoard b =
    filter (\ x -> x < 1 || x > 9) b == [] &&
    (size . fromList) b == 9 -- uniqueness and length

-- |The goal of the game.
endBoard :: Board
endBoard = [1 .. 9]

-- |Indices of the board
type Position = Int -- only 0..8

-- |Index of 9 in the list
pos9 :: Board -> Position
pos9 b = maybe (error "invalid Board") id (elemIndex 9 b) 

-- |9 can move up,down,left,right
data Direction = Up | Dn | Lt | Rt deriving (Show, Eq, Read)

-- |All directions in which 9 can move, given a certain Board
possibleDirections :: Board -> [Direction]
possibleDirections b = 
    let p = pos9 b in
    [] ++
    (if p > 2 then [Up] else []) ++
    (if p < 6 then [Dn] else []) ++    
    (if p `mod` 3 > 0 then [Lt] else []) ++
    (if p `mod` 3 < 2 then [Rt] else [])

-- |Changes the given board by moving 9 in the given direction
--  No change if the given direction is not possible.
move :: Direction -> Board -> Board
move d b
    | not (d `elem` possibleDirections b) = b
    | d == Up   = swapListElems p9 (p9-3) b
    | d == Dn   = swapListElems p9 (p9+3) b
    | d == Lt   = swapListElems p9 (p9-1) b
    | d == Rt   = swapListElems p9 (p9+1) b
    where p9 = pos9 b

-- |Executes a sequence of moves. 
--  The moves are executed from left to right
moves :: [Direction] -> Board -> Board
moves ds b = foldl' (flip move) b ds

{- 2. Helper functions for IO -}

-- |Compact representation of a board
board2Str :: Board -> String
board2Str = map (\ n -> (head . show) n)

-- |Board from compact representation
str2Board :: String -> Maybe Board
str2Board str =
    if isBoard b then Just b else Nothing 
    where b = map (\ c -> ord c - ord '0') str

-- |Compact representation of a direction
dir2Str :: Direction -> String
dir2Str Up = "U"
dir2Str Dn = "D"
dir2Str Lt = "L"
dir2Str Rt = "R"

-- |Compact representation of a sequence of directions
dirs2Str :: [Direction] -> String   
dirs2Str = concat . ( map dir2Str )

-- |Dir from compact representation
str2Dir :: String -> Maybe Direction
str2Dir = undefined

-- |Dirs from compact representation
str2Dirs :: String -> Maybe [Direction]
str2Dirs = undefined

-- |Shows changes of a board by a sequence of moves
showMoves :: Board -> [Direction] -> String 
showMoves b = fst . ( foldl' appendMove (board2Str b, b) ) where
    appendMove :: (String,Board) -> Direction -> (String,Board)
    appendMove (s,b) d = (s',b') where
        b' = move d b
        s' = s ++ " " ++ (dir2Str d) ++ "\n" ++ (board2Str b')

