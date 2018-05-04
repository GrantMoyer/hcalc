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

------------
-- Parser --
------------

-- Laguage Grammar --
--
-- Line -> Expression | {}
-- Expression -> Term Application
-- Term -> NumToken | LParen Expression RParen
-- Application -> OpToken Term Application | {}

type AST a = LineNode a
type LineNode a = Maybe (ExpNode a)

data ExpNode a = Exp (TermNode a) (AppNode a) deriving(Show)

data TermNode a = NumTerm a
                | ParTerm (ExpNode a)
                deriving(Show)

type AppNode a = Maybe (AppNodeDef a)
data AppNodeDef a = App Op (TermNode a) (AppNode a) deriving(Show)

-- use a recursive descent parser
parseAST = parseLine

parseLine :: [Token a] -> ((LineNode a), [Token a])
parseLine [] = (Nothing, [])
parseLine ts = (\(v, ts) -> (Just v, ts)) $ parseExp ts

parseExp :: [Token a] -> ((ExpNode a), [Token a])
parseExp ts = let (term, rem) = parseTerm ts
                  (app, rem') = parseApp rem
              in (Exp term app, rem')

parseTerm :: [Token a] -> ((TermNode a), [Token a])
parseTerm ((NumToken x):ts) = (NumTerm x, ts)
parseTerm ((LParen):ts) = let (exp, (RParen):rem) = parseExp ts in (ParTerm exp, rem)

parseApp :: [Token a] -> ((AppNode a), [Token a])
parseApp ((OpToken op):ts) = let (term, rem) = parseTerm ts
                                 (app, rem') = parseApp rem
                             in (Just $ App op term app, rem')
parseApp ts = (Nothing, ts)


parse :: [Token a] -> (AST a)
parse = fst . parseAST

--------------------------------------
-- AST -> Evaluation Tree Transform --
--------------------------------------

data EvalTree a = Operation (EvalTree a) Op (EvalTree a)
                | Number a
                deriving(Show)

fromAST :: AST a -> Maybe (EvalTree a)
fromAST = fromLineNode

fromLineNode :: LineNode a -> Maybe (EvalTree a)
fromLineNode = fmap fromExpNode

fromExpNode :: ExpNode a -> EvalTree a
fromExpNode (Exp term app) = fromAppNode (fromTermNode term) app

fromTermNode :: TermNode a -> EvalTree a
fromTermNode (NumTerm x) = Number x
fromTermNode (ParTerm exp) = fromExpNode exp

fromAppNode :: EvalTree a -> AppNode a -> EvalTree a
fromAppNode x Nothing = x
fromAppNode x (Just (App op term app)) = fromAppNode (Operation x op (fromTermNode term)) app

----------------
-- Evaluation --
----------------

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

interpret :: String -> String
interpret = unlines . map interpretLine . lines

interpretLine :: String -> String
interpretLine = maybeShow . eval .fromAST . parse . scan
    where maybeShow Nothing = ""
          maybeShow (Just x) = show x
