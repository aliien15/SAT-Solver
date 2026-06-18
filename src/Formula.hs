module Formula (
    Formula (..)
) where

data Formula
    = Var String
    | Value Bool
    | Not Formula
    | And Formula Formula
    | Or Formula Formula
    deriving (Show, Eq, Read)