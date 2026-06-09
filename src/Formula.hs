module Formula (
    Formula (..)
) where

data Formula
    = Var String
    | Not Formula
    | And Formula Formula
    | Or Formula Formula
    deriving (Show, Eq)