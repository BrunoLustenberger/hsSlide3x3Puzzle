{- |
Module : GameMaps
Description : A map describing all possible boards and their solutions.
Copyright : (c) 2022 Bruno M.S. Lustenberger

The entire game is described here by a map containing all boards, that
can be generated from the endBoard. The boards themselves are used as keys
(using the default ordering of integer lists). The value of each key resp. 
board is its pedigree, i.e. the shortest sequence of moves that generates 
this board from the endBoard.

Building the entire map is done stepwise. We start with the endBoard in 
the map. Then the 1st generation is created and its boards and pedigrees 
are inserted into the map. Then the 2nd generation and so on. After creating
a generation from the previous one, we first delete all duplicates from
that generation and then all boards, that already occur in the map. The 
remaining boards are inserted into the map and also serve as parents for
the next generation. This process finally halts, when all boards of the
current generation already occur in the map.

Once the entire map is built, it is easy to "solve" any board: you lookup
the board in the map, get its pedigree and switch the directions. Applying
the moves of the resulting list to the board you will get the endBoard.

Example:
Looking up the board 943/216/758 in the map gives pedigree 
    [Lt,Up,Rt,Dn,Lt,Up,Up,Lt]   (last move at head)
The solution for this board is
    [Rt,Dn,Lt,Up,Rt,Dn,Dn,Rt]   (first move at head)

The map also shows that not every permutation of (1,2,3,4,5,6,7,8,9) 
represents a board that can be generated fromthe endBoard. It contains only
181440 boards, i.e. half of 9! = 362880.

Furthermore, the longest pedigree has 31 moves. Thus every valid board
can be solved with at most 31 moves.
-}
module GameMaps
{- Uncomment for tests
    ( solveBoard
    , gameMapSize
    , maxPedigreeLength
    ) -}
where

import qualified Data.Map as M

import GeneralUtils (whileLoop)

import Boards
import Generations

{- 1. Building the map -}

-- |(Board,Pedigree) as (key,value) pair
type BoardMap = M.Map Board Pedigree

-- |The field map contains all boards generated so far.
--  The field generation contains the last processed generation, but only 
--  those boards, that were inserted anew into the map, all others have 
--  been discarded.
data GameMapBuildStep = GameMapBuildStep {
      generation :: Generation
    , boardMap :: BoardMap
    } deriving (Show, Eq)

-- |The initial state
initGameMapBuildStep = GameMapBuildStep {
      generation = [exEndBoard]
    , boardMap = M.singleton (board exEndBoard) (pedigree exEndBoard)
    }

-- |Deletes all boards from a generation, that already occur in the map
reduceGen :: BoardMap -> Generation -> Generation
reduceGen bm = filter (\ eb -> board eb `M.notMember` bm)

-- |Converts a generation into a board map
convertGen :: Generation -> BoardMap
convertGen = M.fromList . ( map (\ eb -> (board eb, pedigree eb))  )

-- |Inserts all boards from a generation into the map
insertGen :: Generation -> BoardMap -> BoardMap
insertGen = M.union . convertGen 

-- |Performs 1 step in the GameMap generation
nextStep :: GameMapBuildStep -> GameMapBuildStep
nextStep gmbs = GameMapBuildStep generation' boardMap' where
    generation' = reduceGen (boardMap gmbs) $ 
                            (compressGen . sortGen . nextGen) (generation gmbs)
    boardMap' = insertGen generation' (boardMap gmbs)

-- |Checks, whether the last step has been reached, 
--  i.e. the entire map has been built
isEndStep :: GameMapBuildStep -> Bool
isEndStep = null . generation

-- |The final step, no new generations are created
finitGameMapBuildStep = whileLoop (not.isEndStep) nextStep initGameMapBuildStep

-- |The map containig all possible boards
gameMap = boardMap finitGameMapBuildStep

{- 2. Solving the game -}

-- |Finds a board in the map, if present
lookupBoard :: Board -> Maybe Pedigree
lookupBoard board = M.lookup board gameMap 

-- |Reverses a single direction
reverseDir :: Direction -> Direction
reverseDir d
    | d == Up  = Dn
    | d == Dn  = Up
    | d == Lt  = Rt
    | d == Rt  = Lt

-- |Returns the list of moves, that turn the board into the endBoard
--  First move is head of the list
solveBoard :: Board -> Maybe [Direction]
solveBoard = ((fmap . map) reverseDir) . lookupBoard

{- 3.Other properties of the game -}

-- |The size of the entire map
gameMapSize = M.size gameMap

-- |All steps, as an infinite sequence
allSteps :: [GameMapBuildStep]
allSteps = initGameMapBuildStep : next allSteps
  where
    next :: [GameMapBuildStep] -> [GameMapBuildStep]
    next (x : xs) = (nextStep x) : next xs

-- |The sizes of the generations and maps
allSizes :: [(Int, Int)]
allSizes = map (\ x -> ((length . generation) x, (M.size . boardMap) x)) 
           allSteps

-- |The maximal length of a pedigree
maxPedigreeLength :: Int
maxPedigreeLength = length ( takeWhile (\ x -> fst x > 0) allSizes ) - 1

-- |A simple example of an impossible board, 
--  i.e. it cannot be reached from endBoard
endBoard' :: Board
endBoard' = [1,2,3,4,5,6,8,7,9]

-- todo: generate the other half of boards
