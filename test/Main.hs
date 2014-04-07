import Test.Tasty

import DominionTest
import DominionTest.Cards.Base
import DominionTest.Cards.Original

main = defaultMain tests


tests :: TestTree
tests = testGroup "Tests" [dominionTests, baseCardTests, originalCardTests]

