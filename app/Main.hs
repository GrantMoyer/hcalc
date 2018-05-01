module Main where

import Hcalc(interpret)

main :: IO ()
main = interact $ interpret
