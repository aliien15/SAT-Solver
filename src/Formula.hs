module Formula (
    Formula (..)
) where

import Test.QuickCheck

data Formula
    = Var String
    | Value Bool
    | Not Formula
    | And Formula Formula
    | Or Formula Formula
    deriving (Show, Eq, Read)

-- Logic that handles the generation of random Formulas
instance Arbitrary Formula where
    arbitrary = sized formulaSized

-- Helper function for the Arbitrary instance of Formulas
formulaSized :: Int -> Gen Formula
formulaSized 0 = do
    name <- elements ["A", "B", "C", "D", "E"]
    return $ Var name
formulaSized n =
    frequency
    [
        (3, formulaSized 0),
        (1, do
            let newSize = n - 1
            f <- formulaSized newSize
            return $ Not f),
        (1, do
            let halfSize = n `div` 2
            firstFormula <- formulaSized halfSize
            secondFormula <- formulaSized halfSize
            return $ And firstFormula secondFormula),
        (1, do
            let halfSize = n `div` 2
            firstFormula <- formulaSized halfSize
            secondFormula <- formulaSized halfSize
            return $ Or firstFormula secondFormula)
    ]