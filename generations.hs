module Generations where

import Data.List (sortBy)

import Boards

type Pedigree = [Direction] -- Most recent move at head of list
-- |An extended board contains also the moves, by which it was generated.
--  These moves can be considered as the pedigree of the board and the  
--  endBoard as the prime ancestor. 
data ExBoard = ExBoard {
      board :: Board
    , pedigree :: Pedigree 
    } deriving (Show, Eq)

-- |Moves the board extending its pedigree
generate :: Direction -> ExBoard -> ExBoard
generate d (ExBoard b p) = ExBoard (move d b) (d:p)

exEndBoard = ExBoard [1,2,3,4,5,6,7,8,9] []

-- |The endBoard is the prime ancestor. Its children are the 1st generation,
--  their children are the 2nd generation and so on.
--  Thus, a generation consists of extended boards, which all have a pedigree
--  of the same length.
type Generation = [ExBoard]

-- |all boards that can be generated with 1 move from a given board
children :: ExBoard -> Generation
children e = foldr (\ d es -> (generate d e) : es) [] 
                   (possibleDirections (board e))

-- |Generates the next generation from a given generation
nextGen :: Generation -> Generation
nextGen = concat . (map children)

-- |Extended Boards are ordered by comparing the contained board.
compareEB :: ExBoard -> ExBoard -> Ordering
compareEB eb1 eb2
    | board eb1 < board eb2    = LT
    | board eb1 < board eb2    = EQ
    | otherwise                = GT

-- |Sorts a generation according to the ordering of extended boards.
sortGen :: Generation -> Generation
sortGen = sortBy compareEB

-- |Consecutive elements containing the same board are replaced
--  by the first element. Thus, if the generation has been sorted before,
--  all duplicates are removed.
compressGen :: Generation -> Generation
compressGen = foldr upd [] where
    upd :: ExBoard -> Generation -> Generation
    upd e [] = [e]
    upd e (e1:es) = if board e == board e1 then (e:es) else (e:e1:es)

