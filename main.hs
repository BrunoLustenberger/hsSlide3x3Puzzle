{- |
Module : Main
Description : Solves any board of the 3x3 slide game.
Copyright : (c) 2022 Bruno M.S. Lustenberger

...


You can compile this module into a main program like so:
$ ghc main.hs
Then you can call this program e.g. like so:
$ ./main


-}
module Main (
      main
    ) where

import GeneralUtils (simpleDialogLoop, SimpleDialog)
import GameMaps
import Boards

prompt = ">"
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
--    | Shuffle [Direction] 
--    | Verbose Boolean 

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
    str <- getLine
    let command = decodeCommand str
    case command of
        Solve b -> do
            -- (putStrLn . show . solveBoard) b
            let res = maybe "impossible board" (\ ds -> show ds) (solveBoard b)
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


