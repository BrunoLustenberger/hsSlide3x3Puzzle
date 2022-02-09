{- |
Module : GeneralUtils
Description : General utilities
Copyright : (c) 2022 Bruno M.S. Lustenberger
-}
module GeneralUtils where

{- Lists -}

-- |Test out of range for index of a list.
outOfRange :: Int -> [a] -> Bool
outOfRange i xs = i < 0 || i >= length xs

-- |Set the value at an index of the list.
--  No change if index out of range.
setListElem :: Int -> a -> [a] -> [a]
setListElem i x xs
    | outOfRange i xs   = xs
    | otherwise         = xs1 ++ x:xs2 where
        xs1 = take i xs
        xs2 = drop (i+1) xs
 
-- |Swap 2 list elements at given indices.
--  No change if an index is out of range 
swapListElems :: Int -> Int -> [a] -> [a]
swapListElems i1 i2 xs
    | outOfRange i1 xs || outOfRange i2 xs   = xs
    | otherwise = setListElem i1 (xs !! i2) (setListElem i2 (xs !! i1) xs) 


{- Loops as functions -}

{- |Simulating a while loop, e.g. in python
        x = init
        while condition(x):
            x = next(x)
    Assuming, that the loop ends, this can be considered as a function 
    that computes for each init value a finit value.
    Note that the type of x can be arbitrarily complex.
-}
whileLoop :: (a -> Bool) -> (a -> a) -> a -> a
whileLoop condition next x = 
    if condition x then whileLoop condition next (next x) 
                   else x

-- |A dialog with a user. The dialog returns True 
--  iff after completion a next dialog should be done. 
type SimpleDialog = IO Bool

simpleDialogExample :: IO Bool
simpleDialogExample = do
    putStrLn "type anything, q means quit"
    str <- getLine
    if str == "q" then return False
                  else do
                      putStrLn ("simulating " ++ str)
                      return True

{- |Simple dialog loop via stdin, stdout -}
simpleDialogLoop :: SimpleDialog -> IO ()
simpleDialogLoop simpleDialog = do
    continue <- simpleDialog
    if continue then simpleDialogLoop simpleDialog
                else return () 



