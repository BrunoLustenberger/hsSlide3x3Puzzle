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

prompt = ".."

data Command = 
      Solve Board 
    | Shuffle [Direction] 
--    | Verbose Boolean 
    | Quit 
    | Invalid String

decodeCommand :: String -> Command
decodeCommand str = 
    let ws = words str in
    case ws of 
        ["q"] -> Quit
{-
        ["s",ts] -> maybe (Invalid "Input string is not a board") 
                          (\ b -> Solve b) (str2Board ts) 
-}        
        ["s",ts] -> case str2Board ts of
                        Just b  -> Solve b
                        Nothing -> Invalid "Input string is not a board"
        otherwise -> Invalid "invalid input or not yet implemented"

simpleDialog :: SimpleDialog
simpleDialog = do
    putStrLn prompt
    str <- getLine
    let command = decodeCommand str
    case command of
        Solve b -> do
            (putStrLn . show . solveBoard) b
            return True
        Shuffle ds -> do
            putStrLn ("simulating ....")
            return True
        Quit -> return False
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


