module Hcalc.Eval
  (eval
  ) where

import Hcalc.Op(Op(..))
import Hcalc.Parser(EvalTree(..))

eval :: Fractional a => Maybe (EvalTree a) -> Maybe a
eval = fmap eval' where
    eval' (Number x) = x
    eval' (Operation x op y) = let p = eval' x
                                   q = eval' y
                               in case op of
                                   Sum -> p + q
                                   Difference -> p - q
                                   Product -> p * q
                                   Quotient -> p / q
