module Hcalc
  ( interpret
  ) where

import Data.Char(isSpace, generalCategory)
import Data.Function(on)
import Data.List(groupBy)
import Data.Maybe(fromMaybe, fromJust, isJust)
import Data.Tuple(swap)

-----------
-- Lexer --
-----------

-- Strings must be tokenized into:
-- * Operations
-- * Numbers
-- * Parentheses

-- Represent various mathmatical operations
data Op = Sum
        | Difference
        | Product
        | Quotient
        deriving(Eq)

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

data Token = Number Double
           | Operation Op
           | LParen
           | RParen
           deriving(Show)

readParens :: String -> [(Token, String)]
readParens s = let stripped = dropWhile isSpace s
                   firstChar = if null stripped then Nothing else Just . head $ stripped
                   remaining = if isJust firstChar then tail stripped else stripped
               in case firstChar of
                   Just '(' -> [(LParen, remaining)]
                   Just '[' -> [(LParen, remaining)]
                   Just ')' -> [(RParen, remaining)]
                   Just ']' -> [(RParen, remaining)]
                   _        -> []

instance Read Token where
  readsPrec _ s = let toNum = (\(v, s) -> (Number v, s))
                      toOp = (\(v, s) -> (Operation v, s))
                      nums = map toNum $ (readsPrec 0 :: ReadS Double) s
                      ops = map toOp $ (readsPrec 0 :: ReadS Op) s
                      parens = readParens s
                  in ops ++ nums ++ parens

scan :: String -> [Token]
scan "" = []
scan s = let rs = (readsPrec 0 :: ReadS Token) s
             (t, rem) = head rs
         in t : scan rem

------------
-- Parser --
------------

-- Laguage Grammar --
--
-- Expression -> Term Application
-- Term -> Number | LParen Expression RParen
-- Application -> Op Term Application | {}

data ExpNode = Exp TermNode AppNode deriving(Show)

data TermNode = NumTerm Double
              | ParTerm ExpNode
              deriving(Show)

type AppNode = Maybe AppNodeDef
data AppNodeDef = App Op TermNode AppNode deriving(Show)

-- use a recursive descent parser
parseExp :: [Token] -> (ExpNode, [Token])
parseExp ts = let (term, rem) = parseTerm ts
                  (app, rem') = parseApp rem
              in (Exp term app, rem')

parseTerm :: [Token] -> (TermNode, [Token])
parseTerm ((Number x):ts) = (NumTerm x, ts)
parseTerm ((LParen):ts) = let (exp, (RParen):rem) = parseExp ts in (ParTerm exp, rem)

parseApp :: [Token] -> (AppNode, [Token])
parseApp ((Operation op):ts) = let (term, rem) = parseTerm ts
                                   (app, rem') = parseApp rem
                               in (Just $ App op term app, rem')
parseApp ts = (Nothing, ts)


parse :: [Token] -> ExpNode
parse = fst . parseExp

interpret :: String -> String
interpret = unlines . map interpretLine . lines

interpretLine :: String -> String
interpretLine = show .{-print . eval .-} parse . scan

--print :: Val -> String

--eval :: AST -> Val

