module Main where

import Test.QuickCheck
import Formula
import Normalize
import Solver ()
import TestUtils

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

-- Main entry point to run the tests
main :: IO ()
main = do
    putStrLn "\n=== Running SAT Solver Property Tests ==="
    
    putStrLn "Testing Solver Validity..."
    quickCheck prop_solverValid
    
    putStrLn "\nTesting Contradiction Handling..."
    quickCheck prop_contradiction
    
    putStrLn "\nTesting Normalization Equivalence..."
    quickCheck prop_normalization
    
    putStrLn "=== All tests completed! ===\n"