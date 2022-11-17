module Main where

import Lib (foo)
import Test.HUnit (assertEqual)
import Test.QuickCheck (quickCheck)

prop_reverseReverse :: [Char] -> Bool
prop_reverseReverse s = (reverse . reverse) s == s

main :: IO ()
main = do
  quickCheck prop_reverseReverse
  assertEqual "Test of equality" foo "Hello, World!"
