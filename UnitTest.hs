import Test.HUnit

import qualified HBF.Test.TestVM as TestVM
import qualified HBF.Test.TestTapeList as TestTapeList

tests = [
    TestLabel "VM" $ TestList TestVM.tests, 
    TestLabel "TapeList" $ TestList TestTapeList.tests
    ]

main = do
    runTestTT $ TestList tests