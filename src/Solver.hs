module Solver where

import Formula

-- Type that holds the variables from a Formula data type (as a String) with their value (a Bool)
type Assignment = [(String, Bool)]

-- Looks up a variable from a formula in an Assignment, and returns its Bool value
lookupVar :: String -> Assignment -> Bool
lookupVar x assign = case lookup x assign of
    Just val -> val
    Nothing  -> False

-- Evaluates whether a Formula can be true or not based on an assignment
eval :: Assignment -> Formula -> Bool
eval assign (Var x)     = lookupVar x assign
eval assign (Not f)     = not $ eval assign f
eval assign (And f1 f2) = eval assign f1 && eval assign f2
eval assign (Or f1 f2)  = eval assign f1 || eval assign f2

-- Gets all the variables from a Formula and returns a list with them, making sure all variable only pop up in the
-- list once (even if they can be seen multiple times in the formula)
getVars :: Formula -> [String]
getVars (Var x)     = [x]
getVars (Not f)     = getVars f
getVars (And f1 f2) = nub $ getVars f1 ++ getVars f2
getVars (Or f1 f2)  = nub $ getVars f1 ++ getVars f2