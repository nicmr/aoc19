import Test.Tasty
import Test.Tasty.HUnit
import Text.Megaparsec

import Data.List (intersect, minimumBy)


import qualified Day3
import Day3 (Segment(..), Direction(..))

main :: IO ()
main = do
  defaultMain (testGroup "Day3 tests" [manhattan, parsing1, endToEnd1, endToEnd2])

endToEnd1 =
    testCase "testing end to end with example1"
        (either (\error -> assertFailure error)
                (\ok -> assertEqual "example1:" example1Solution ok)
                (Day3.task1' example1)
        )

endToEnd2 =
    testCase "testing end to end with example2"
        (either (\error -> assertFailure error)
                (\ok -> assertEqual "example2:" example2Solution ok)
                (Day3.task1' example2)
        )

manhattan =
    testCase "testing manhattanOrdering function"
    ( assertEqual "Should be 2,4" (2,4) $ minimumBy Day3.manhattanOrdering [(7,7), (8,5), (2,4)]
    )

parsing1 =
    testCase "Testing parsing of ex1"
        (either (\error -> assertFailure $ show error)
                (\ok -> assertEqual "example 1 parsing:" example1Parsed ok)
                (parse Day3.parseContents "" example1)
        )

-- parseTest1 :: TestTree
-- parseTest1 = testCase("Testing parsing with input one")

example1 = "R75,D30,R83,U83,L12,D49,R71,U7,L72\nU62,R66,U55,R34,D71,R55,D58,R83"
example1Solution = 159
example1Parsed =
    [   [(Segment GoRight 75), (Segment GoDown 30) , (Segment GoRight 83), (Segment GoUp 83), (Segment GoLeft 12)
        ,(Segment GoDown 49), (Segment GoRight 71), (Segment GoUp 7), (Segment GoLeft 72)
        ]
    ,   [(Segment GoUp 62), (Segment GoRight 66), (Segment GoUp 55), (Segment GoRight 34), (Segment GoDown 71)
        ,(Segment GoRight 55), (Segment GoDown 58), (Segment GoRight 83)
        ]
    ]


example2 = "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51\nU98,R91,D20,R16,D67,R40,U7,R15,U6,R7"
example2Solution = 135