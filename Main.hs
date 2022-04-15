module Main where

import           StringCalculator   (stringCalc)
import           System.Environment

main :: IO ()
main = concat <$> getArgs >>= print . stringCalc
