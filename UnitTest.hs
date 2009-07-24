import Test.HUnit

import qualified HBF.Test.TestVM as TestVM
import qualified HBF.Test.TestTapeList as TestTapeList
import qualified HBF.Test.TestCharIO as TestCharIO

tests = [
    TestLabel "VM" $ TestList TestVM.tests, 
    TestLabel "TapeList" $ TestList TestTapeList.tests, 
    TestLabel "TestCharIO" $ TestList TestCharIO.tests
    ]

main = do
    runTestTT $ TestList tests