module HBF.Test.TestTapeList where

import Test.HUnit
import HBF.TapeList

emptyBegin = TestCase $ assertBool "Empty TP is at begin" ( beginTP $ makeTP [] ) 
emptyEnd = TestCase $ assertBool "Empty TP is at end" ( endTP $ makeTP [] ) 

intList = makeTP [33, 44, 55] :: TapeList Int
int2List = fwdTP intList
int3List = fwdTP int2List
int4List = fwdTP int3List

lists = [intList, int2List, int3List, int4List]

tests = [ 
    -- makeTP is already tested...
    -- getEntireTP
    "getEntireTP" ~: getEntireTP int2List ~=? [33, 44, 55],
    "getEntireTP" ~: (all ( \tp -> getEntireTP tp == [33, 44, 55] ) lists) ~=? True,

    --leftTP, rightTP
    "leftTP" ~: rightTP intList ~=? [33, 44, 55],
    "leftTP" ~: leftTP int3List ~=? [44, 33],
    "rightTP" ~: rightTP int3List ~=? [55],

    -- beginTP, endTP
    TestCase $ ( beginTP $ makeTP [] ) @? "Empty TP is at begin", 
    TestCase $ ( endTP $ makeTP [] ) @? "Empty TP is at end", 
    TestCase $ ( map beginTP lists == [True, False, False, False]) @? "beginTP",
    TestCase $ ( map endTP lists == [False, False, False, True]) @? "endTP",

    -- getTP, fwdTP, backTP
    "Getting First element" ~: ( getTP intList ) ~=? 33, 
    "Forward and get 2nd element" ~: ( getTP $ fwdTP intList ) ~=? 44,
    "Back and get 1st element" ~: ( getTP $ backTP int2List ) ~=? 33,
    
    -- putTP, 
    "Prepare" ~: ( getTP int2List ) ~=? 44,
    "putTP" ~: ( getEntireTP $ putTP int2List 440) ~=? [33, 440, 55],
    "applyTP" ~: ( getEntireTP $ applyTP int2List (*2)) ~=? [33, 88, 55]
    
    ]
