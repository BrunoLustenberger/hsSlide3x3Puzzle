{- |
Module : GenerationsUTests
Description : Unit testcases for module Generations
Copyright : (c) 2022 Bruno M.S. Lustenberger

You can run these unit testcases from ghci by loading this module 
and using the command runTestTT,e.g.

> runTestTT ch00 -- for a single test case
> runTestTT childrenTests -- for a group of test cases
> runTestTT allTestCases -- for all test cases
-}
module GenerationsUTests where

import Test.HUnit

import Boards
import Generations

{- children -}


ch00 = TestCase $ assertEqual "ch00" ( children (ExBoard [1,2,3,4,5,6,7,8,9] []) ) 
    [  ExBoard [1,2,3,4,5,9,7,8,6] [Up], ExBoard [1,2,3,4,5,6,7,9,8] [Lt] ]
ch01 = TestCase $ assertEqual "ch01" (children ( ExBoard [1,2,3,4,5,9,7,8,6] [Up]) )
    [  ExBoard [1,2,9,4,5,3,7,8,6] [Up,Up], ExBoard [1,2,3,4,5,6,7,8,9] [Dn,Up], 
       ExBoard [1,2,3,4,9,5,7,8,6] [Lt,Up] ]
ch02 = TestCase $ assertEqual "ch02" (children ( ExBoard [1,2,3,4,5,6,7,9,8] [Lt]) )
    [  ExBoard [1,2,3,4,9,6,7,5,8] [Up,Lt], ExBoard [1,2,3,4,5,6,9,7,8] [Lt,Lt], 
       ExBoard [1,2,3,4,5,6,7,8,9] [Rt,Lt] ]
ch03 = TestCase $ assertEqual "ch03" (children ( ExBoard [1,2,3,4,9,6,7,8,5] [Lt,Up]) )
    [  ExBoard [1,9,3,4,2,6,7,8,5] [Up,Lt,Up], ExBoard [1,2,3,4,8,6,7,9,5] [Dn,Lt,Up], 
       ExBoard [1,2,3,9,4,6,7,8,5] [Lt,Lt,Up], ExBoard [1,2,3,4,6,9,7,8,5] [Rt,Lt,Up] ]

childrenTests = TestLabel "children" (TestList 
    [ch00,ch01,ch02,ch03])


{- generations -}

gen0 = [exEndBoard]
gen1 = nextGen gen0
gen2 = nextGen gen1
gen3 = nextGen gen2

gen1' = sortGen gen1
gen1'' = compressGen gen1'
gen2' = sortGen gen2
gen2'' = compressGen gen2'
gen3' = sortGen gen3
gen3'' = compressGen gen3'


g01 = TestCase $ assertEqual "g01" gen1
    [  ExBoard [1,2,3,4,5,9,7,8,6] [Up], ExBoard [1,2,3,4,5,6,7,9,8] [Lt] ]
g01' = TestCase $ assertEqual "g01'" gen1'
    [  ExBoard [1,2,3,4,5,6,7,9,8] [Lt], ExBoard [1,2,3,4,5,9,7,8,6] [Up] ]
g01'' = TestCase $ assertEqual "g01''" gen1' gen1''

g02 = TestCase $ assertEqual "g02" gen2
    [  ExBoard [1,2,9,4,5,3,7,8,6] [Up,Up], ExBoard [1,2,3,4,5,6,7,8,9] [Dn,Up], 
       ExBoard [1,2,3,4,9,5,7,8,6] [Lt,Up],
       ExBoard [1,2,3,4,9,6,7,5,8] [Up,Lt], ExBoard [1,2,3,4,5,6,9,7,8] [Lt,Lt], 
       ExBoard [1,2,3,4,5,6,7,8,9] [Rt,Lt] ]
g02' = TestCase $ assertEqual "g02'" gen2'
    [  ExBoard [1,2,3,4,5,6,7,8,9] [Rt,Lt], ExBoard [1,2,3,4,5,6,7,8,9] [Dn,Up], 
            -- order of EQ elems is reversed!
       ExBoard [1,2,3,4,5,6,9,7,8] [Lt,Lt],  
       ExBoard [1,2,3,4,9,5,7,8,6] [Lt,Up],
       ExBoard [1,2,3,4,9,6,7,5,8] [Up,Lt],
       ExBoard [1,2,9,4,5,3,7,8,6] [Up,Up]
    ]
g02'' = TestCase $ assertEqual "g02''" gen2''
    [  ExBoard [1,2,3,4,5,6,7,8,9] [Rt,Lt], 
       ExBoard [1,2,3,4,5,6,9,7,8] [Lt,Lt],  
       ExBoard [1,2,3,4,9,5,7,8,6] [Lt,Up],
       ExBoard [1,2,3,4,9,6,7,5,8] [Up,Lt],
       ExBoard [1,2,9,4,5,3,7,8,6] [Up,Up]
    ]

g03 = TestCase $ assertEqual "g03" gen3
    [  -- ExBoard [1,2,9,4,5,3,7,8,6] [Up,Up]
       ExBoard [1,2,3,4,5,9,7,8,6] [Dn,Up,Up], 
       ExBoard [1,9,2,4,5,3,7,8,6] [Lt,Up,Up],
       -- ExBoard [1,2,3,4,5,6,7,8,9] [Dn,Up]
       ExBoard [1,2,3,4,5,9,7,8,6] [Up,Dn,Up], 
       ExBoard [1,2,3,4,5,6,7,9,8] [Lt,Dn,Up],
       -- ExBoard [1,2,3,4,9,5,7,8,6] [Lt,Up]
       ExBoard [1,9,3,4,2,5,7,8,6] [Up,Lt,Up],
       ExBoard [1,2,3,4,8,5,7,9,6] [Dn,Lt,Up],
       ExBoard [1,2,3,9,4,5,7,8,6] [Lt,Lt,Up],
       ExBoard [1,2,3,4,5,9,7,8,6] [Rt,Lt,Up],
       -- ExBoard [1,2,3,4,9,6,7,5,8] [Up,Lt]
       ExBoard [1,9,3,4,2,6,7,5,8] [Up,Up,Lt],
       ExBoard [1,2,3,4,5,6,7,9,8] [Dn,Up,Lt],
       ExBoard [1,2,3,9,4,6,7,5,8] [Lt,Up,Lt],
       ExBoard [1,2,3,4,6,9,7,5,8] [Rt,Up,Lt],
       -- ExBoard [1,2,3,4,5,6,9,7,8] [Lt,Lt]
       ExBoard [1,2,3,9,5,6,4,7,8] [Up,Lt,Lt],
       ExBoard [1,2,3,4,5,6,7,9,8] [Rt,Lt,Lt],
       -- ExBoard [1,2,3,4,5,6,7,8,9] [Rt,Lt]
       ExBoard [1,2,3,4,5,9,7,8,6] [Up,Rt,Lt],
       ExBoard [1,2,3,4,5,6,7,9,8] [Lt,Rt,Lt]
    ]
g03' = TestCase $ assertEqual "g03'" gen3'
    [  
       ExBoard [1,2,3,4,5,6,7,9,8] [Lt,Rt,Lt],
       ExBoard [1,2,3,4,5,6,7,9,8] [Rt,Lt,Lt],
       ExBoard [1,2,3,4,5,6,7,9,8] [Dn,Up,Lt],    
       ExBoard [1,2,3,4,5,6,7,9,8] [Lt,Dn,Up],

       ExBoard [1,2,3,4,5,9,7,8,6] [Up,Rt,Lt],
       ExBoard [1,2,3,4,5,9,7,8,6] [Rt,Lt,Up],
       ExBoard [1,2,3,4,5,9,7,8,6] [Up,Dn,Up],
       ExBoard [1,2,3,4,5,9,7,8,6] [Dn,Up,Up], 

       ExBoard [1,2,3,4,6,9,7,5,8] [Rt,Up,Lt],

       ExBoard [1,2,3,4,8,5,7,9,6] [Dn,Lt,Up],

       ExBoard [1,2,3,9,4,5,7,8,6] [Lt,Lt,Up],

       ExBoard [1,2,3,9,4,6,7,5,8] [Lt,Up,Lt],

       ExBoard [1,2,3,9,5,6,4,7,8] [Up,Lt,Lt],

       ExBoard [1,9,2,4,5,3,7,8,6] [Lt,Up,Up],

       ExBoard [1,9,3,4,2,5,7,8,6] [Up,Lt,Up],

       ExBoard [1,9,3,4,2,6,7,5,8] [Up,Up,Lt]

    ]

g03'' = TestCase $ assertEqual "g03''" gen3''
    [  
       ExBoard [1,2,3,4,5,6,7,9,8] [Lt,Rt,Lt],

       ExBoard [1,2,3,4,5,9,7,8,6] [Up,Rt,Lt],

       ExBoard [1,2,3,4,6,9,7,5,8] [Rt,Up,Lt],

       ExBoard [1,2,3,4,8,5,7,9,6] [Dn,Lt,Up],

       ExBoard [1,2,3,9,4,5,7,8,6] [Lt,Lt,Up],

       ExBoard [1,2,3,9,4,6,7,5,8] [Lt,Up,Lt],

       ExBoard [1,2,3,9,5,6,4,7,8] [Up,Lt,Lt],

       ExBoard [1,9,2,4,5,3,7,8,6] [Lt,Up,Up],

       ExBoard [1,9,3,4,2,5,7,8,6] [Up,Lt,Up],

       ExBoard [1,9,3,4,2,6,7,5,8] [Up,Up,Lt]
    ]


generationsTests = TestLabel "generations" (TestList 
    [g01,g01',g01'', g02,g02',g02'', g03,g03',g03''])


{- all tests -}

allTestCases = TestList [childrenTests, generationsTests]


