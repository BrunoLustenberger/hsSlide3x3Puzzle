{- |
Module : GameMapsPTests
Description : Property tests for module GameMapsPTests
Copyright : (c) 2022 Bruno M.S.  Lustenberger

You can run these tests from ghci by loading this module and then
calling the functions quickCheck or verboseCheck for these properties.
Examples:
> quickCheck propPossible -- quickCheck, 100 tests
> quickCheck $ withMaxSuccess 1000  propPossible -- quickCheck, 1000 tests
> verboseCheck propPossible -- verboseCheck, 100 tests
> verboseCheck $ withMaxSuccess 1000  propPossible -- verboseCheck, 1000 tests

-}
module GameMapsPTests 
    (  propPossible
    ,  propImpossible
    )
where

import Test.QuickCheck

import Boards
import GameMaps

int2Dir :: Int -> Direction
int2Dir n = case n `mod` 4 of
                0 -> Up
                1 -> Dn
                2 -> Lt
                3 -> Rt

--  generating Directions arbitrarily
instance Arbitrary Direction where
    arbitrary = do
        n <- arbitrary
        return (int2Dir n)

--  moves with flipped arguments
moves' :: Board -> [Direction] -> Board
moves' = flip moves

--  moves' with 2 arguments mapped to Maybe
moves'' :: Board -> Maybe [Direction] -> Maybe Board
moves'' board = fmap (moves' board)

-- |Solve any board reachable from endBoard
--  The board to solve is generated from endBoard and a list of directions
propPossible :: [Direction] -> Bool
propPossible ds = moves'' board (solveBoard board) == Just endBoard where 
    board = moves ds endBoard

-- |Solve any board not reachable from endBoard
--  The board to solve is generated from endBoard' and a list of directions
propImpossible :: [Direction] -> Bool
propImpossible ds = moves'' board (solveBoard board) == Nothing where 
    board = moves ds endBoard'


