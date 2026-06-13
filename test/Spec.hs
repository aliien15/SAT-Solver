module Spec where

import Test.QuickCheck
import Formula (Formula(..))
import Normalize
import Solver

-- Logic that handles the generation of random Formulas
instance Arbitrary Formula where
    arbitrary :: Gen Formula
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

-- Checks if every set of solutions (Assignments) is valid
prop_solverValid :: Formula -> Bool
prop_solverValid f = all (\assign -> eval assign f) solutions
    where
        solutions = solve f

-- Checks if contradictions/Formulas with no solutions are handled accordingly
prop_contradiction :: Formula -> Bool
prop_contradiction f = null $ solve $ And f $ Not f

-- Check if normalizing a Formula doesn't change its logical value/the solutions remain the same
prop_normalization :: Formula -> Bool
prop_normalization f = normalizedSolution && originalSolution
    where
        normalized = cnf $ nnf f
        
        -- Prove the normalizer didn't invent fake solutions
        normalizedSolution = all (\assign -> eval assign f) (solve normalized)
        
        -- Prove the normalizer didn't delete valid solutions
        originalSolution = all (\assign -> eval assign normalized) (solve f)