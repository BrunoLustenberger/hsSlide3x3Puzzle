module GameMapsUTests where

import Test.HUnit
import qualified Data.Map as M

import Boards
import Generations
import GameMaps

{- GameMapBuildState -}

state1 = nextState initGameMapBuildState
state2 = nextState state1
state3 = nextState state2

-- see also the values in GenerationsUTests

st1 = TestCase $ assertEqual "st1" state1
    GameMapBuildState 
        {
            generation = 
            [  ExBoard [1,2,3,4,5,6,7,9,8] [Lt], 
               ExBoard [1,2,3,4,5,9,7,8,6] [Up] 
            ]
        ,   boardMap = M.fromList 
            [  ([1,2,3,4,5,6,7,8,9], []),
               ([1,2,3,4,5,6,7,9,8], [Lt]), 
               ([1,2,3,4,5,9,7,8,6], [Up]) 
            ]
        }

st2 = TestCase $ assertEqual "st2" state2
    GameMapBuildState 
        {
            generation = 
            [   -- ExBoard [1,2,3,4,5,6,7,8,9] [Rt,Lt], 
                ExBoard [1,2,3,4,5,6,9,7,8] [Lt,Lt],  
                ExBoard [1,2,3,4,9,5,7,8,6] [Lt,Up],
                ExBoard [1,2,3,4,9,6,7,5,8] [Up,Lt],
                ExBoard [1,2,9,4,5,3,7,8,6] [Up,Up]
            ]
        ,   boardMap = M.fromList 
            [  ([1,2,3,4,5,6,7,8,9], []),
               ([1,2,3,4,5,6,7,9,8], [Lt]), 
               ([1,2,3,4,5,9,7,8,6], [Up]),
               --
               ([1,2,3,4,5,6,9,7,8], [Lt,Lt]),  
               ([1,2,3,4,9,5,7,8,6], [Lt,Up]),
               ([1,2,3,4,9,6,7,5,8], [Up,Lt]),
               ([1,2,9,4,5,3,7,8,6], [Up,Up])
            ]
        }

st3 = TestCase $ assertEqual "st3" state3
    GameMapBuildState 
        {
            generation = 
            [   -- ExBoard [1,2,3,4,5,6,7,9,8] [Lt,Rt,Lt],
                -- ExBoard [1,2,3,4,5,9,7,8,6] [Up,Rt,Lt],
                ExBoard [1,2,3,4,6,9,7,5,8] [Rt,Up,Lt],
                ExBoard [1,2,3,4,8,5,7,9,6] [Dn,Lt,Up],
                ExBoard [1,2,3,9,4,5,7,8,6] [Lt,Lt,Up],
                ExBoard [1,2,3,9,4,6,7,5,8] [Lt,Up,Lt],
                ExBoard [1,2,3,9,5,6,4,7,8] [Up,Lt,Lt],
                ExBoard [1,9,2,4,5,3,7,8,6] [Lt,Up,Up],
                ExBoard [1,9,3,4,2,5,7,8,6] [Up,Lt,Up],
                ExBoard [1,9,3,4,2,6,7,5,8] [Up,Up,Lt]
            ]

        ,   boardMap = M.fromList 
            [  ([1,2,3,4,5,6,7,8,9], []),
               ([1,2,3,4,5,6,7,9,8], [Lt]), 
               ([1,2,3,4,5,9,7,8,6], [Up]),
               --
               ([1,2,3,4,5,6,9,7,8], [Lt,Lt]),  
               ([1,2,3,4,9,5,7,8,6], [Lt,Up]),
               ([1,2,3,4,9,6,7,5,8], [Up,Lt]),
               ([1,2,9,4,5,3,7,8,6], [Up,Up]),
               --
               ([1,2,3,4,6,9,7,5,8], [Rt,Up,Lt]),
               ([1,2,3,4,8,5,7,9,6], [Dn,Lt,Up]),
               ([1,2,3,9,4,5,7,8,6], [Lt,Lt,Up]),
               ([1,2,3,9,4,6,7,5,8], [Lt,Up,Lt]),
               ([1,2,3,9,5,6,4,7,8], [Up,Lt,Lt]),
               ([1,9,2,4,5,3,7,8,6], [Lt,Up,Up]),
               ([1,9,3,4,2,5,7,8,6], [Up,Lt,Up]),
               ([1,9,3,4,2,6,7,5,8], [Up,Up,Lt])
            ]
        }

st10 = TestCase $ assertEqual "st10" finitGameMapBuildState
    ( (head . (dropWhile (not . isEndState))) allStates )
st11 = TestCase $ assertEqual "st11" (boardMap finitGameMapBuildState)
    ( boardMap ((last . (takeWhile (not . isEndState))) allStates) )


gmbsTests = TestLabel "gmbs" (TestList 
    [st1,st2,st3,st10,st11])


{- lookup and solve -}

ls00  = TestCase $ assertEqual "ls00" (lookupBoard [1,2,3,4,5,6,7,8,9]) 
    (Just [])
ls00' = TestCase $ assertEqual "ls00'" (solveBoard [1,2,3,4,5,6,7,8,9])
    (Just [])
ls01  = TestCase $ assertEqual "ls01" (lookupBoard [1,2,3,4,5,6,7,9,8]) 
    (Just [Lt])
ls01'  = TestCase $ assertEqual "ls01'" (solveBoard [1,2,3,4,5,6,7,9,8]) 
    (Just [Rt])
ls02  = TestCase $ assertEqual "ls02" (lookupBoard [1,2,3,4,6,9,7,5,8]) 
    (Just [Rt,Up,Lt])
ls02' = TestCase $ assertEqual "ls02'" (solveBoard [1,2,3,4,6,9,7,5,8]) 
    (Just [Lt,Dn,Rt])

lookupSolveTests = TestLabel "lookupSolve" (TestList 
    [ls00,ls01,ls01',ls02,ls02'])


{- all tests -}

allTestCases = TestList [gmbsTests, lookupSolveTests]


