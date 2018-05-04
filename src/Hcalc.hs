module Hcalc
  (interpret
  ) where

import Hcalc.Eval(eval)
import Hcalc.Lexer(scan)
import Hcalc.Parser(parse)

interpret :: String -> String
interpret = unlines . map interpretLine . lines

interpretLine :: String -> String
interpretLine = maybeShow . eval . parse . scan
    where maybeShow Nothing = ""
          maybeShow (Just x) = show x
