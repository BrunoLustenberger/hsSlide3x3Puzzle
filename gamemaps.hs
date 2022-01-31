module GameMaps where

import qualified Data.Map as M

import GeneralUtils (whileLoop)

import Boards
import Generations


type BoardMap = M.Map Board Pedigree

-- |Building the entire game tree is done stepwise. After each step,
--  the game tree is in a certain state. The field map contains all 
--  boards generated so far. Each board is unique and has the shortest 
--  possible pedigree leading to this board. The field generation
--  contains the last processed generation, but only those boards,
--  that were inserted anew into the map, all others have been discarded.
data GameMapBuildState = GameMapBuildState {
      generation :: Generation
    , boardMap :: BoardMap
    } deriving (Show, Eq)

-- |The initial state
initGameMapBuildState = GameMapBuildState {
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
nextState :: GameMapBuildState -> GameMapBuildState
nextState gmbs = GameMapBuildState generation' boardMap' where
    generation' = reduceGen (boardMap gmbs) $ 
                            (compressGen . sortGen . nextGen) (generation gmbs)
    boardMap' = insertGen generation' (boardMap gmbs)

-- |Checks, whether end state has been reached, 
--  i.e. the entire tree has bee built
isEndState :: GameMapBuildState -> Bool
isEndState = null . generation

-- |The final state, no new generations are created
finitGameMapBuildState = whileLoop (not.isEndState) nextState initGameMapBuildState

-- |The map containig all possible boards
gameMap = boardMap finitGameMapBuildState

-- |The size of the entire map
gameMapSize = M.size gameMap

-- |All states, as an infinite sequence
allStates :: [GameMapBuildState]
allStates = initGameMapBuildState : next allStates
  where
    next :: [GameMapBuildState] -> [GameMapBuildState]
    next (x : xs) = (nextState x) : next xs

-- |The sizes of the generations and maps
allSizes :: [(Int, Int)]
allSizes = map (\ x -> ((length . generation) x, (M.size . boardMap) x)) 
           allStates

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

