{- |
Module : Main
Description : Solves any board of the 3x3 tile game.
Copyright : (c) 2022 Bruno M.S. Lustenberger

The program first builds the map of all boards and
then presents a simple dialog loop. The user can enter
any board, the program then looks up the solution,
if any, and presents it to the user as a series
of moves.

Example:
$ ./main
Generating map .. 181440 boards

> s 123456978
RR
123456978 R
123456798 R
123456789

> s 346758912
UURDDRUULDLURDDRUULDRD
346758912 U
346958712 U
946358712 R
496358712 D
456398712 D
456318792 R
456318729 U
456319728 U
459316728 L
495316728 D
415396728 L
415936728 U
915436728 R
195436728 D
135496728 D
135426798 R
135426789 U
135429786 U
139425786 L
193425786 D
123495786 R
123459786 D
123456789

> s 346758921
Impossible board

You can compile this module into a main program like so:
$ ghc main.hs
Then you can call this program e.g. like so:
$ ./main
Alternatively you can call the program from ghci:
ghci> :l main.hs
ghci> main


-}
module Main (
      main
    ) where

import System.IO (stdout, hFlush)

import GeneralUtils (simpleDialogLoop, SimpleDialog)
import GameMaps
import Boards

prompt = "> "
help   = "s board\n" ++
         "    to solve a board\n" ++
         "    board is entered e.g. like so: 123456978\n" ++
         "h\n" ++
         "    for help\n" ++
         "q\n" ++
         "    to quit"

data Command = 
      Solve Board 
    | Quit 
    | Help
    | Invalid String
-- to implement later
--  | Shuffle [Direction] 

decodeCommand :: String -> Command
decodeCommand str = 
    case words str of 
        ["q"] -> Quit
{-
        ["s",ts] -> maybe (Invalid "Input string is not a board") 
                          (\ b -> Solve b) (str2Board ts) 
-}        
        ["s",ts] -> case str2Board ts of
            Just b  -> Solve b
            Nothing -> Invalid "Input string is not a board"
        "h":s -> Help
        "?":s -> Help
        otherwise -> Invalid "invalid input, type h for help"

simpleDialog :: SimpleDialog
simpleDialog = do
    putStrLn ""
    putStr prompt
    hFlush stdout
    str <- getLine
    let command = decodeCommand str
    case command of
        Solve b -> do
            let res = case solveBoard b of
                    Nothing -> "Impossible board"
                    Just ds -> dirs2Str ds ++ "\n" ++ showMoves b ds
            putStrLn res
            return True
        Quit -> return False
        Help -> do
            putStrLn help
            return True
        Invalid s -> do
            putStrLn s
            return True

-- |Main program.
main :: IO ()
main = do
    putStr "Generating map .. "
    let s = gameMapSize
    putStrLn (show s ++ " boards")
    simpleDialogLoop simpleDialog


