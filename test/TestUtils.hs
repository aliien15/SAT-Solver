module TestUtils where

import Formula
import Solver

--------------------------------------------------------------------------------------------------------------
-- This is a slower implementation I made when I first built this, but I kept this here for testing reason  --
-- since dpll is way too abstract to be properly and accurately tested                                      --
--------------------------------------------------------------------------------------------------------------

-- Type that holds the variables from a Formula data type (as a String) with their value (a Bool)
type Assignment = [(String, Bool)]

-- Looks up a variable from a formula in an Assignment, and returns its Bool value
lookupVar :: String -> Assignment -> Bool
lookupVar x assign = case lookup x assign of
    Just val -> val
    Nothing  -> False

-- Evaluates whether a Formula can be true or not based on an assignment
eval :: Assignment -> Formula -> Bool
eval _ (Value b) = b
eval assign (Var x)         = lookupVar x assign
eval assign (Not f)         = not $ eval assign f
eval assign (And f1 f2)     = eval assign f1 && eval assign f2
eval assign (Or f1 f2)      = eval assign f1 || eval assign f2
eval assign (Implies f1 f2) = not (eval assign f1) || eval assign f2
eval assign (Iff f1 f2)     = eval assign f1 == eval assign f2

-- Takes a list of variables and generates every possible combination of True and False with them
genAssignments :: [String] -> [Assignment]
genAssignments [] = [[]]
genAssignments (v:vs) = [ (v, val) : rest | val <- [True, False], rest <- genAssignments vs ]

-- Takes a formula and generates a list of Assignments that makes the Formula evaluate to true
solve :: Formula -> [Assignment]
solve formula = 
    let
        vars = getVars formula
        assignments = genAssignments vars
    in
        filter (\assign -> eval assign formula) assignments