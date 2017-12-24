module Syntax where

type Name = String

data Expr
  = Lit Double
  | Var Name
  | BinOp Op Expr Expr
  | Function Name [Name] Expr
  | Call Name [Expr]
  deriving (Eq, Ord, Show)

data Op
  = Mul
  | Div
  | Add
  | Sub
  deriving (Eq, Ord, Show)