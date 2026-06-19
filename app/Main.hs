module Main where

import Formula
import Normalize
import Solver
import Text.Read (readMaybe)
import System.Exit (exitSuccess)

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
            -- runPerformanceBenchMark
            putStrLn "This option is not available yet :("
            main
        3 -> do
            putStrLn "Shutting down, goodbye!"
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