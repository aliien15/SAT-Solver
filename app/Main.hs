module Main where

import Formula
import Normalize
import Solver
import Text.Read (readMaybe)
import System.Exit (exitSuccess)
import System.CPUTime
import Text.Printf

import Control.Exception (evaluate)
import Control.Monad (forM_)
import Data.IORef

-- Default formulas used for the performance benchmarks
quickFormula :: Formula
quickFormula = 
    And (Or (Var "A") (Var "B")) 
        (And (Not (Var "A")) 
             (Or (Var "C") (And (Var "D") (Var "E"))))

pigeonholeFormula :: Formula
pigeonholeFormula =
    And (Or (Var "P11") (Var "P12"))
    (And (Or (Var "P21") (Var "P22"))
    (And (Or (Var "P31") (Var "P32"))
    (And (Not (And (Var "P11") (Var "P21")))
    (And (Not (And (Var "P11") (Var "P31")))
    (And (Not (And (Var "P21") (Var "P31")))
    (And (Not (And (Var "P12") (Var "P22")))
    (And (Not (And (Var "P12") (Var "P32")))
         (Not (And (Var "P22") (Var "P32"))))))))))

nightmareFormula :: Formula
nightmareFormula = 
    And (Or (Not (Var "A")) (Or (Not (Var "B")) (Var "C"))) 
    (And (Or (Var "A") (Or (Not (Var "B")) (Var "C"))) 
    (And (Or (Not (Var "A")) (Or (Var "B") (Var "C"))) 
    (And (Or (Var "A") (Or (Var "B") (Not (Var "C")))) 
    (And (Or (Not (Var "A")) (Or (Not (Var "B")) (Not (Var "C")))) 
    (And (Or (Var "A") (Or (Not (Var "B")) (Not (Var "C")))) 
    (And (Or (Not (Var "A")) (Or (Var "B") (Not (Var "C")))) 
         (Or (Var "A") (Or (Var "B") (Var "C")))))))))

-- Main function: Parses the arguments and calls the corresponding mode function
main :: IO ()
main = do
    putStrLn "=== SAT Solver ==="
    putStrLn "1. Run Standard Solver"
    putStrLn "2. Run Performance Benchmark"
    putStrLn "3. Exit"
    option <- getValue (1, 3) "Select an option: "

    case option of
        1 -> do
            runSolver
            main
        2 -> do
            runPerformanceBenchMark
            main
        3 -> do
            putStrLn "\nShutting down, goodbye!"
            exitSuccess

-- Function that keeps asking the user for an input until it is valid
getValue :: (Int, Int) -> String -> IO Int
getValue (minValue, maxValue) msg = do
    putStrLn msg
    input <- getLine
    case readMaybe input :: Maybe Int of
        Just v | v >= minValue && v <= maxValue -> return v
        _ -> do
            putStrLn $ "Invalid value! Enter a number between " ++ show minValue ++ " and " ++ show maxValue ++ "!"
            getValue (minValue, maxValue) msg

-- Function that runs all the formula solving logic
runSolver :: IO ()
runSolver = do
    putStrLn "\n=== Interactive Formula Builder ==="
    putStrLn "Type your formula using strict constructors (And, Or, Not, Var, Value)."
    putStrLn "Example: And (Var \"A\") (Or (Not (Var \"A\")) (Var \"B\"))"
    putStrLn "(Type 'exit' to return to the Main Menu)"
    putStr "Enter formula: "
    
    input <- getLine

    if input == "exit"
        then putStrLn "Returning to the main menu...\n"
    else case readMaybe input :: Maybe Formula of
        Just formula -> do
            putStrLn "\nAnalyzing your formula..."

            let isSatisfiable = dpll $ cnf $ nnf formula
            putStrLn $ "\nResult: " ++ if isSatisfiable then "SATISFIABLE\n" else "UNSATISFIABLE\n"

            runAgain <- getValue (1, 3) "Do you want to try another formula?\n1. Yes!\n2. No, exit to main menu!\n3. No, exit the program!"
            case runAgain of
                1 -> runSolver
                2 -> do
                    putStrLn "Returning to the main menu...\n"
                3 -> do
                    putStr "Shutting down, goodbye!"
                    exitSuccess
            
        Nothing -> do
            putStrLn "\n[Syntax Error] Make sure you are using exact capitalization and parentheses!"
            runSolver

-- Function that runs all the performance testing logic
runPerformanceBenchMark :: IO ()
runPerformanceBenchMark = do
    putStrLn "\n=== Performance Benchmark ==="
    putStrLn "1. Quick Test (Satisfiable - 5 Variables)"
    putStrLn "2. Deep Backtracking (Unsatisfiable - Pigeonhole Principle)"
    putStrLn "3. The Nightmare (Symmetric Conflict - Extreme Backtracking)"
    putStrLn "4. Cancel"
    
    choice <- getValue (1, 4) "Select a benchmark tier: "
    case choice of
        1 -> runTimedBenchmark "Quick Test" quickFormula
        2 -> runTimedBenchmark "Pigeonhole" pigeonholeFormula
        3 -> runTimedBenchmark "The Nightmare" nightmareFormula
        4 -> putStrLn "Returning to Main Menu...\n"

-- Solves a formula and prints the time it took in seconds
runTimedBenchmark :: String -> Formula -> IO ()
runTimedBenchmark name f = do
    putStrLn $ "\nRunning " ++ name ++ "..."
    
    start <- getCPUTime
    
    -- evaluate forces the pure calculation to happen inside the IO boundary
    _ <- evaluate (dpll (cnf (nnf f)))
    
    end <- getCPUTime
    
    let diff = fromIntegral (end - start) / (1e12) :: Double
    let finalResult = dpll (cnf (nnf f))
    
    putStrLn $ "Result: " ++ if finalResult then "SATISFIABLE" else "UNSATISFIABLE"
    printf "Execution Time: %0.12f seconds\n\n" diff