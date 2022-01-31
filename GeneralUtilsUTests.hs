{- |
Module : GeneralUtilsUTests
Description : Unit tests for module GeneralUtils
Copyright : (c) 2022 Bruno M.S. Lustenberger

You can execute these tests from ghci by loading this module 
and then enter e.g.:
ghci> runTestTT st0 -- for a single testcase
ghci> runTestTT outOfRangeTests -- for a group of testcases
ghci> runTestTT allTestCases -- for all testcases 
-}
module GeneralUtilsUTests where

import Test.HUnit

import GeneralUtils

{- Lists -}

or0 = TestCase $ assertEqual "or0" (outOfRange 0 []) True 
or1 = TestCase $ assertEqual "or1" (outOfRange 2 []) True 
or2 = TestCase $ assertEqual "or2" (outOfRange 1 [3]) True 
or3 = TestCase $ assertEqual "or3" (outOfRange 0 [3]) False 
or4 = TestCase $ assertEqual "or4" (outOfRange 0 [3,4,2]) False 
or5 = TestCase $ assertEqual "or5" (outOfRange 2 [3,4,2]) False 
or6 = TestCase $ assertEqual "or6" (outOfRange (-1) [3,4,2]) True 
or7 = TestCase $ assertEqual "or7" (outOfRange 3 [3,4,2]) True 

outOfRangeTests = TestLabel "outOfRange" (TestList 
    [or0,or1,or2,or3,or4,or5,or6,or7])

st0 = TestCase $ assertEqual "st0" (setListElem 0 9 [1,2,3]) [9,2,3] 
st1 = TestCase $ assertEqual "st1" (setListElem 1 9 [1,2,3]) [1,9,3] 
st2 = TestCase $ assertEqual "st2" (setListElem 2 9 [1,2,3]) [1,2,9] 
st3 = TestCase $ assertEqual "st3" (setListElem 5 9 [1,2,3]) [1,2,3] 
st4 = TestCase $ assertEqual "st4" (setListElem (-3) 9 [1,2,3]) [1,2,3] 

setElemTests = TestLabel "setListElem" (TestList 
    [st0,st1,st2,st3,st4])

sw0 = TestCase $ assertEqual "sw0" (swapListElems 0 9 [1,2,3]) [1,2,3]
sw1 = TestCase $ assertEqual "sw1" (swapListElems (-2) 2 [1,2,3]) [1,2,3]
sw2 = TestCase $ assertEqual "sw2" (swapListElems 1 1 [1,2,3]) [1,2,3]
sw3 = TestCase $ assertEqual "sw3" (swapListElems 2 0 [1,2,3]) [3,2,1]

swapElemsTests = TestLabel "swapListElems" (TestList 
    [sw0,sw1,sw2,sw3])


{- Loops as functions -}

{-
find the next multiple of 7
x = 9
while (x % 7 != 0):
    x = x+1
-}
wlm7 :: Int -> Int
wlm7 = whileLoop (\ x -> (x `mod` 7) /= 0) (\ x -> x+1)

lf0 = TestCase $ assertEqual "lf0" (wlm7 9) 14
lf1 = TestCase $ assertEqual "lf1" (wlm7 (-3)) 0

{- factorial variants -}
fact :: Int -> Int
fact n = snd $ whileLoop (\ (x,res) -> x > 1) (\ (x,res) -> (x-1,x*res)) (n,1)
{-  don't work, ?
fact n = (snd . whileLoop) 
        ((\ (x,res) -> x > 1)::(Int,Int)->Bool) 
        ((\ (x,res) -> (x-1,x*res))::(Int,Int)->(Int,Int)) (n,1)
-}
lf2 = TestCase $ assertEqual "lf2" (fact 1) 1
lf3 = TestCase $ assertEqual "lf3" (fact 0) 1
lf4 = TestCase $ assertEqual "lf4" (fact (-4)) 1
lf5 = TestCase $ assertEqual "lf5" (fact 2) 2
lf6 = TestCase $ assertEqual "lf6" (fact 3) 6
lf7 = TestCase $ assertEqual "lf7" (fact 5) 120
lf8 = TestCase $ assertEqual "lf8" (fact 10) (product [1..10])

loopsAsFunctionsTests = TestLabel "loopsAsFunctions" (TestList 
    [lf0,lf1,lf2,lf3,lf4,lf5,lf6,lf7,lf8])


{- all tests -}

allTestCases = TestList [outOfRangeTests, setElemTests, swapElemsTests,
    loopsAsFunctionsTests]
