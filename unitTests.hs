import Test.HUnit
import Data.List( sort )
import Webrexp.Exprtypes

nodeRangeTests :: [Test]
nodeRangeTests =
    [ "Basic  conservation" ~: [Index 1] ~=? simplifyNodeRanges [Index 1]
    , "Basic conservation2" ~: [Interval 3 4] ~=? simplifyNodeRanges [Interval 3 4]
    , "Basic conservation2" ~: [] ~=? simplifyNodeRanges []
    , "Sorting 1" ~: [Interval 1 2, Index 4, Index 6] ~=? sort [Index 6, Index 4, Interval 1 2]
    , "Sorting 2" ~: [Interval 1 2, Index 1, Index 6] ~=? sort [Index 6, Index 1, Interval 1 2]
    , "Sorting 3" ~: [Interval 1 2, Interval 1 3, Index 1, Index 6] ~=? sort [Interval 1 3, Index 6, Index 1, Interval 1 2]
    , "Simplification" ~: [Interval 1 3, Index 6] ~=? simplifyNodeRanges [Interval 1 3, Index 6, Index 1, Interval 1 2]
    , "Simplification2" ~: [Interval 1 3, Index 6, Index 7] ~=? simplifyNodeRanges [Index 6, Interval 1 3, Index 6, Index 7, Index 1, Interval 1 2]
    ]
tests = TestList nodeRangeTests

main :: IO ()
main = do
    _ <- runTestTT tests
    return ()

