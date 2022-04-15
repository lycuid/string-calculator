module Main where

import           StringCalculator (CalcError (..), stringCalc)
import           Test.HUnit

main :: IO Counts
main  = runTestTT
      $ TestList
      [ mkTest ""               (Right 0)
      , mkTest "//;\n"          (Right 0)
      , mkTest "//;"            (Left $ MalformedString "//;")
      , mkTest "a1,2"           (Left $ MalformedString "a1,2")
      , mkTest "1,2"            (Right 3)
      , mkTest "1,2\n3"         (Right 6)
      , mkTest "1\n2,3"         (Right 6)
      , mkTest "1\n2\n3"        (Right 6)
      , mkTest "1,\n2"          (Left $ MalformedString ",\n2")
      , mkTest "1\n,2"          (Left $ MalformedString "\n,2")
      , mkTest "//\n1;2\n3"     (Left $ MalformedString "//\n1;2\n3")
      , mkTest "//;\n1;2\n3"    (Right 6)
      , mkTest "//;\n1\n2;3"    (Right 6)
      , mkTest "//;\n1\n2,3"    (Left $ MalformedString ",3")
      , mkTest "//;\n1\n-2;3"   (Left $ NegativeNumbers [-2])
      , mkTest "//;\n1\n-2;-3"  (Left $ NegativeNumbers [-2, -3])
      ]
  where
    mkTest lhs = TestCase . assertEqual ("Input: '" ++ lhs ++ "'") (stringCalc lhs)
