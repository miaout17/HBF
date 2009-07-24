module HBF.Test.TestVM where

import Test.HUnit
import HBF.VM

bfFileTC :: String -> String -> String -> Test
bfFileTC filename input expectOutput = TestCase $ do
    code <- readFile filename
    output <- return $ runMockBF code input
    assertEqual ("BF File: "++filename) output expectOutput

tests = [ 
        bfFileTC "HelloWorld.bf" "" "Hello world!\n", 
        bfFileTC "JABH.bf" "" "Just another brainfuck hacker.",
        bfFileTC "Text2BF.bf" "yellow" "+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++.>+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++.>++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++.>++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++.>+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++.>+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++.>",
        TestCase $ do
            result <- return $ runMockBF ",>++++++++++++++++++++++++++[-<.+>]" "a"
            assertEqual "Input" ['a'..'z'] result
    ]
