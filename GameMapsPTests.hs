module GameMapsPTests where

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

moves' :: Board -> [Direction] -> Board
moves' = flip moves

moves'' :: Board -> Maybe [Direction] -> Maybe Board
moves'' board = fmap (moves' board)

-- |solve any board reachable from endBoard
propXXX :: [Direction] -> Bool
propXXX ds = moves'' board (solveBoard board) == Just endBoard where 
    board = moves ds endBoard

-- |solve any board not reachable from endBoard
propYYY :: [Direction] -> Bool
propYYY ds = moves'' board (solveBoard board) == Nothing where 
    board = moves ds endBoard'


