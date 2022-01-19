module GeneralUtilsUTests where

import Test.HUnit

import GeneralUtils

{- lists -}

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

{- all tests -}

allTestCases = TestList [outOfRangeTests, setElemTests, swapElemsTests]
