module Hcalc.Lexer
  (scan
  ,Token(..)
  ) where

import Data.Char(isSpace, generalCategory)
import Data.Function(on)
import Data.List(groupBy)
import Data.Maybe(fromMaybe, fromJust, isJust)
import Data.Tuple(swap)

import Hcalc.Op(Op(..))

-- Strings must be tokenized into:
-- * Operations
-- * Numbers
-- * Parentheses, left and right

-- The string representation of each type of operation
-- Should have entry for each Op constructor
opStrings :: [(Op, String)]
opStrings = [(Sum, "+")
            ,(Difference, "-")
            ,(Product, "*")
            ,(Quotient, "/")
            ]

instance Read Op where
  readsPrec _ s = let groups = groupBy ((==) `on` generalCategory) s
                      firstWord = head . filter (not . all isSpace) $ groups
                      remaining = drop (length firstWord) . dropWhile isSpace $ s
                      rLookup x = lookup x . map swap
                      operation = rLookup firstWord opStrings
                      pack op = [(op, remaining)]
                  in fromMaybe [] . fmap pack $ operation

-- can use fromJust since opStrings should have an entry for every Op constructor
instance Show Op where
  showsPrec _ op = fromJust . fmap (++) $ lookup op opStrings

data Token a = NumToken a
             | OpToken Op
             | LParen
             | RParen
             deriving(Show)

readParens :: String -> [((Token a), String)]
readParens s = let stripped = dropWhile isSpace s
                   firstChar = if null stripped then Nothing else Just . head $ stripped
                   remaining = if isJust firstChar then tail stripped else stripped
               in case firstChar of
                   Just '(' -> [(LParen, remaining)]
                   Just '[' -> [(LParen, remaining)]
                   Just ')' -> [(RParen, remaining)]
                   Just ']' -> [(RParen, remaining)]
                   _        -> []

instance (Read a) => Read (Token a) where
  readsPrec _ s = let toNum = (\(v, s) -> (NumToken v, s))
                      toOp = (\(v, s) -> (OpToken v, s))
                      nums = map toNum $ readsPrec 0 s
                      ops = map toOp $ readsPrec 0 s
                      parens = readParens s
                  in ops ++ nums ++ parens

scan :: (Read a) => String -> [Token a]
scan "" = []
scan s = let rs = readsPrec 0 s
             (t, rem) = head rs
         in t : scan rem

