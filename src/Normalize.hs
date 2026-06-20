module Normalize where

import Formula

-- NNF (negation normal form) pushes all the Nots as far down the tree as possible, so they only attach directly to variables (e.g., Not (Var "A")).
nnf :: Formula -> Formula
nnf (Var x)               = Var x
nnf (Value b)             = Value b
nnf (Not (Value b))       = Value (not b)
nnf (Not (Var x))         = Not (Var x)
nnf (Not (Not f))         = nnf f
nnf (Implies f1 f2)       = nnf $ Or (Not f1) f2
nnf (Not (Implies f1 f2)) = nnf (And f1 (Not f2))
nnf (Iff f1 f2)           = nnf $ And (Implies f1 f2) (Implies f2 f1)
nnf (Not (Iff f1 f2))     = nnf (Or (And f1 (Not f2)) (And f2 (Not f1)))
-- Recursively apply nnf to the insides of normal ANDs/ORs:
nnf (And f1 f2)           = And (nnf f1) (nnf f2)
nnf (Or f1 f2)            = Or (nnf f1) (nnf f2)
-- De Morgan
nnf (Not (And f1 f2))     = Or (nnf (Not f1)) (nnf (Not f2))
nnf (Not (Or f1 f2))      = And (nnf (Not f1)) (nnf (Not f2))

-- CNF (conjuctive normal form) pushes all the Ors downwards so they sit exclusively underneath the Ands.
-- The formula must be in NNF before passing through this
cnf :: Formula -> Formula
cnf (Value b)     = Value b
cnf (Var x)       = Var x
cnf (Not (Var x)) = Not (Var x)
-- Recursively apply cnf to the insides of normal ANDs/ORs:
cnf (And f1 f2)   = And (cnf f1) (cnf f2)
cnf (Or f1 f2)    = distrib (cnf f1) (cnf f2)
-- Error
cnf (Not _)       = error "CNF: Formula not in NNF!"

-- Takes two CNF formulas and forces them together using the distributive property.
distrib :: Formula -> Formula -> Formula
distrib (And g1 g2) f = And (distrib g1 f) (distrib g2 f)
distrib f (And g1 g2) = And (distrib f g1) (distrib f g2)
distrib f1 f2         = Or f1 f2