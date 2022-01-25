{- |
Module : BoardsUTests
Description : Unit tests for module Boards
Copyright : (c) 2022 Bruno M.S. Lustenberger

You can execute these tests from ghci by loading this module 
and then enter e.g.:
ghci> runTestTT b00 -- for a single testcase
ghci> runTestTT boardTests -- for a group of testcases
ghci> runTestTT allTestCases -- for all testcases 
-}
module BoardsUTests where

import Test.HUnit

import Boards

{- Boards used in the tests -}

board00 = []
board01 = endBoard
board01' = [5,6,7,1,4,9,8,2,3]
board01'' = [9,6,7,1,4,5,8,2,3]
board02 = [5,6,7,10,4,9,8,2,3]
board03 = [5,6,7,1,4,9,8,2,12,3]
board04 = [5,6,7,1,4,5,8,2,3]
board05 = [5,6,7,1,9,8,2,3]

board10 = [5,6,7,1,4,3,8,2,9]
board11 = [5,6,7,1,4,3,8,9,2]
board12 = [5,6,7,1,4,3,9,8,2]
board13 = [5,6,7,1,4,9,8,3,2]
board14 = [5,6,7,1,9,3,8,4,2]
board15 = [5,6,7,9,4,3,8,1,2]
board16 = [5,6,9,1,4,3,8,7,2]
board17 = [5,9,7,1,4,3,8,6,2]
board18 = [9,6,7,1,4,3,8,5,2]


{- Board -}

b00 = TestCase $ assertBool "b00" (not (isBoard board00))
b01 = TestCase $ assertEqual "b01" (isBoard board01) True
b01' = TestCase $ assertEqual "b01'" (isBoard board01') True
b02 = TestCase $ assertEqual "b02" (isBoard board02) False
b03 = TestCase $ assertEqual "b03" (isBoard board03) False
b04 = TestCase $ assertEqual "b04" (isBoard board04) False
b05 = TestCase $ assertEqual "b05" (isBoard board05) False

b06 = TestCase $ assertEqual "b06" (pos9 board01) 8
b07 = TestCase $ assertEqual "b07" (pos9 board01') 5
b08 = TestCase $ assertEqual "b08" (pos9 board01'') 0

boardTests = TestLabel "board" (TestList [b00,b01,b01',b02,b03,b04,b05,
                                          b06,b07,b08])

{- Direction -}

d0 = TestCase $ assertEqual "d0" (possibleDirections board10) [Up,Lt]
d1 = TestCase $ assertEqual "d1" (possibleDirections board11) [Up,Lt,Rt]
d2 = TestCase $ assertEqual "d2" (possibleDirections board12) [Up,Rt]
d3 = TestCase $ assertEqual "d3" (possibleDirections board13) [Up,Dn,Lt]
d4 = TestCase $ assertEqual "d4" (possibleDirections board14) [Up,Dn,Lt,Rt]
d5 = TestCase $ assertEqual "d5" (possibleDirections board15) [Up,Dn,Rt]
d6 = TestCase $ assertEqual "d6" (possibleDirections board16) [Dn,Lt]
d7 = TestCase $ assertEqual "d7" (possibleDirections board17) [Dn,Lt,Rt]
d8 = TestCase $ assertEqual "d8" (possibleDirections board18) [Dn,Rt]

directionTests = TestLabel "direction" (TestList 
    [d0,d1,d2,d3,d4,d5,d6,d7,d8])


{- Move -}

m00 = TestCase $ assertEqual "m00" (move Up [5,6,7,1,4,3,8,2,9]) 
                                            [5,6,7,1,4,9,8,2,3]
m01 = TestCase $ assertEqual "m01" (move Dn [5,6,7,1,4,3,8,2,9]) 
                                            [5,6,7,1,4,3,8,2,9]
m02 = TestCase $ assertEqual "m02" (move Lt [5,6,7,1,9,3,8,4,2]) 
                                            [5,6,7,9,1,3,8,4,2]
m03 = TestCase $ assertEqual "m03" (move Rt [5,6,7,1,9,3,8,4,2]) 
                                            [5,6,7,1,3,9,8,4,2]
m04 = TestCase $ assertEqual "m04" (move Up [5,9,7,1,4,3,8,6,2]) 
                                            [5,9,7,1,4,3,8,6,2]
m05 = TestCase $ assertEqual "m05" (move Dn [5,9,7,1,4,3,8,6,2]) 
                                            [5,4,7,1,9,3,8,6,2]
m06 = TestCase $ assertEqual "m06" (move Lt [5,6,7,9,4,3,8,1,2]) 
                                            [5,6,7,9,4,3,8,1,2]
m07 = TestCase $ assertEqual "m07" (move Rt [5,6,7,9,4,3,8,1,2]) 
                                            [5,6,7,4,9,3,8,1,2]

m10 = TestCase $ assertEqual "m10" (moves [] 
    [1,2,3,4,5,6,7,8,9])
    [1,2,3,4,5,6,7,8,9]

m11 = TestCase $ assertEqual "m11" (moves [Lt] 
    [1,2,3,4,5,6,7,8,9])
    [1,2,3,4,5,6,7,9,8]

m12 = TestCase $ assertEqual "m12" (moves [Up,Lt,Dn,Rt,Lt,Lt,Up,Up,Rt] 
    [1,2,3,4,5,6,7,8,9])
    [2,9,3,1,8,5,4,7,6]

moveTests = TestLabel "move" (TestList 
    [m00,m01,m02,m03,m04,m05,m06,m07,m10,m11,m12])

{- all tests -}

allTestCases = TestList [boardTests, directionTests]


