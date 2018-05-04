module Hcalc.Parser
  (EvalTree(..)
  ,parse
  ) where

import Hcalc.Op(Op(..))
import Hcalc.Lexer(Token(..))

-------------------------------------
-- Token sequence -> AST Transform --
-------------------------------------

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
-- Full Parse --
----------------

parse :: [Token a] -> Maybe (EvalTree a)
parse = fromAST . fst . parseAST

