import Test.Tasty

import Tests

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "MP6 Tests" [freshGroup, unifyGroup, inferGroup]
