module Solver where

import Data.List (nub)
import Formula

-- Gets all the variables from a Formula and returns a list with them, making sure all variable only pop up in the
-- list once (even if they can be seen multiple times in the formula)
getVars :: Formula -> [String]
getVars (Var x)     = [x]
getVars (Not f)     = getVars f
getVars (And f1 f2) = nub $ getVars f1 ++ getVars f2
getVars (Or f1 f2)  = nub $ getVars f1 ++ getVars f2
getVars (Implies f1 f2) = nub $ getVars f1 ++ getVars f2
getVars (Iff f1 f2) = nub $ getVars f1 ++ getVars f2
getVars (Value _)   = []

-- Takes a Formula, and replaces a certain variable with its value ("True" or "False")
substitute :: String -> Bool -> Formula -> Formula
substitute _ _ (Value b)       = Value b
substitute var val (Var x)     = if var == x then Value val else Var x
substitute var val (Not x)     = Not $ substitute var val x
substitute var val (And f1 f2) = And (substitute var val f1) (substitute var val f2)
substitute var val (Or f1 f2)  = Or (substitute var val f1) (substitute var val f2)

-- Simplies a Formula based on the information we have of the values of the variable
-- For example, if we have "And True formula" then we can simply it to just "formula"
simplify :: Formula -> Formula
simplify (Var x)               = Var x
simplify (Value val)           = Value val
simplify (Not (Value val))     = Value $ not val
simplify (Not f)               = Not $ simplify f
simplify (And (Value True) f)  = simplify f
simplify (And f (Value True))  = simplify f
simplify (And (Value False) _) = Value False
simplify (And _ (Value False)) = Value False
simplify (And f1 f2)           = And (simplify f1) (simplify f2)
simplify (Or (Value True) _)   = Value True
simplify (Or _ (Value True))   = Value True
simplify (Or (Value False) f)  = simplify f
simplify (Or f (Value False))  = simplify f
simplify (Or f1 f2)            = Or (simplify f1) (simplify f2)

-- If we have a CNF formula like And (Var "A") (Or (Var "B") (Var "C")), we want to instantly lock in "A" = True
-- because it's sitting right out in the open, completely unprotected by an Or. That's what this function does.
findUnitClause :: Formula -> Maybe (String, Bool)
findUnitClause (Var x)       = Just (x, True)
findUnitClause (Not (Var x)) = Just (x, False)
findUnitClause (Value _)     = Nothing
findUnitClause (Or _ _ )     = Nothing
findUnitClause (And f1 f2)   = 
    case findUnitClause f1 of
        Just result -> Just result
        Nothing     -> findUnitClause f2
findUnitClause _             = Nothing -- This shouldn't ever be reached, but just in case

-- The function that ultimately check if a Formula is satisfiable or not
dpll :: Formula -> Bool
dpll (Value val) = val
dpll f = 
    case findUnitClause f of
        -- If we find a logical deduction right away
        Just (var, truth) -> dpll $ simplify $ substitute var truth f
        -- If we dont find a logical deduction right away, therefore we have to guess
        Nothing -> 
            let
                v = head (getVars f)
                trueDpll = dpll $ simplify $ substitute v True f
                falseDpll = dpll $ simplify $ substitute v False f
            in trueDpll || falseDpll