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

-- |solve any board reachable from endBoard
propXXX :: [Direction] -> Bool
propXXX ds = moves'' (solveBoard board) == Just endBoard where 
    board = moves ds endBoard
    moves' = flip moves
    moves'' = fmap (moves' board)

