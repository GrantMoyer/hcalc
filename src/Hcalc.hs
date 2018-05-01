module Hcalc
  ( interpret
  ) where

data OpType = Sum
            | Difference
            | Product
            | Quotient

instance Read OpType where
  readsPrec _ s
    | head s == '+' = [(Sum, tail s)]
    | head s == '-' = [(Difference, tail s)]
    | head s == '*' = [(Product, tail s)]
    | head s == '/' = [(Quotient, tail s)]
    | otherwise = []

instance Show OpType where
  showsPrec _ op = case op of
                     Sum -> ("+" ++)
                     Difference -> ("-" ++)
                     Product -> ("*" ++)
                     Quotient -> ("/" ++)

data Token = Number Double
           | Operation OpType
           deriving(Show)

instance Read Token where
  readsPrec _ s = let opPrec = readsPrec 0 s :: [(OpType, String)]
                  in if null opPrec then map (\(v, s) -> (Number v, s)) (readsPrec 0 s :: [(Double, String)])
                                    else map (\(v, s) -> (Operation v, s)) (readsPrec 0 s :: [(OpType, String)])

interpret :: String -> String
interpret = unlines . map interpretLine . lines

interpretLine :: String -> String
interpretLine = show .{-print . eval . parse .-} scan

--print :: Val -> String

--eval :: AST -> Val

-- parser uses a top down approach
--parse :: [Token] -> AST

scan :: String -> [Token]
scan = map read . words
