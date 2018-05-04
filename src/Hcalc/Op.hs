module Hcalc.Op
  (Op(..)
  ) where

-- Represent various mathmatical operations
data Op = Sum
        | Difference
        | Product
        | Quotient
        deriving(Eq)
