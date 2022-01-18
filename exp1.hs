module Exp1 where

import Data.List (elemIndex)
import Data.Set (fromList, size)

{- 


-}

--  The tiles on the board are marked with the digits 1..8.
--  Digit 9 plays the role of the empty tile.
type Value = Int -- only 1..9

-- |A state of the game is represented by a list of 9 values.
--  The first 3 values represent the top row, 
--  the next 3 values the middle row, the last 3 the bottom row.
--  The list has always length 9 and contains a permutation of 1..9
type Board = [Value]

-- |Checks that the list satisfies the above restrictions.
isBoard :: Board -> Bool
isBoard b =
    -- length b == 9 && .. not necessary
    filter (\ x -> x < 1 || x > 9) b == [] &&
    (size . fromList) b == 9 -- uniqueness and length

-- |The goal of the game is to achieve the "end state" 123/456/789.
endBoard :: Board
endBoard = [1 .. 9]

-- |Indices of the board
type Position = Int -- only 0..8

-- |Index of 9 in the list
pos9 :: Board -> Position
pos9 b = maybe (error "invalid Board") id (elemIndex 9 b) 

-- |9 can move up,down,left,right
data Direction = Up | Dn | Lt | Rt deriving (Show, Eq)

-- |All the directions in which 9 can move, given a certain Board
possibleDirections :: Board -> [Direction]
possibleDirections c = 
    let p = pos9 c in
    [] ++
    (if p > 2 then [Up] else []) ++
    (if p < 6 then [Dn] else []) ++    
    (if p `mod` 3 > 0 then [Lt] else []) ++
    (if p `mod` 3 < 2 then [Rt] else [])

-- |set the value at a position of the board
setVal :: Position -> Value -> Board -> Board
setVal pos val board = b1 ++ val:b2 where
    b1 = take pos board
    b2 = drop (pos+1) board
 
-- |swaps the elements at the two positions
swap :: Position -> Position -> Board -> Board
swap i1 i2 b = setVal i1 (b !! i2) (setVal i2 (b !! i1) b) 

-- |Changes the given board by moving 9 in the given direction
move :: Board -> Direction -> Board
move b d
    | d == Up   = swap p9 (p9-3) b
    | d == Dn   = swap p9 (p9+3) b
    | d == Lt   = swap p9 (p9-1) b
    | d == Rt   = swap p9 (p9+1) b
    where p9 = pos9 b
