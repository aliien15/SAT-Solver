# SAT Solver Engine

A strictly typed Boolean Satisfiability (SAT) solver built entirely from scratch in Haskell. 

Instead of relying on brute-force truth tables, this engine utilizes the highly optimized **DPLL (Davis-Putnam-Logemann-Loveland) algorithm** to logically deduce satisfiability through unit clause propagation and lazy-evaluated branch execution. It features a zero-boilerplate architecture, strictly pure functional data transformations, and an interactive CLI for real-time formula evaluation.

## 🚀 Features

* **DPLL Execution Engine:** Actively hunts for logical deductions and rapidly prunes dead computation branches, completely bypassing the massive $2^n$ memory footprint of standard truth-table generators.
* **Automated Normalization Pipeline:** Seamlessly translates raw abstract syntax trees (ASTs) into Negative Normal Form (NNF) and Conjunctive Normal Form (CNF) before execution.
* **Interactive CLI Builder:** Safely parses Haskell data types at runtime, allowing users to build and stress-test massive logical formulas directly in the terminal without recompiling.
* **Brute-Force Test Oracle:** Includes a mathematically pure reference implementation in the test suite to guarantee strict logical equivalence against the highly-optimized production engine.

---

## 📂 Project Structure

The architecture strictly separates the execution pipeline, data transformation, and testing utilities:

```text
├── app/
│   └── Main.hs           # The CLI front-door, interactive menus, and safe text parsing
├── src/
│   ├── Formula.hs        # The core AST constructors (And, Or, Not, Var, Value)
│   ├── Normalize.hs      # The NNF and CNF translation rules
│   └── Solver.hs         # The master DPLL engine, unit propagation, and substitution logic
├── test/
│   ├── Spec.hs           # Property tests mathematically proving equivalence
│   └── TestUtils.hs      # The brute-force Oracle implementation (eval, solve, genAssignments)
└── sat-solver.cabal
```

---

## 🛠️ Installation & Compilation

### Prerequisites
You will need the **Glasgow Haskell Compiler (GHC)** and a build tool like **Cabal** or **Stack** installed on your machine.
The easiest way to get these is via [GHCup](https://www.haskell.org/ghcup/):
```bash
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```

### Building the Project
1. Clone the repository:
```bash
git clone https://github.com/aliien15/SAT-Solver.git
cd sat-solver
```
2. Compile the executable using Cabal:
```bash
cabal build
```

---

## 🎮 Usage

To launch the interactive engine, run:
```bash
cabal run
```

### The Interactive Formula Builder
When using the sandbox, formulas must be typed using the strict engine constructors: `And`, `Or`, `Not`, `Var`, and `Value`.

**Syntax Examples:**
* A AND B -> `And (Var "A") (Var "B")`
* A OR NOT A -> `Or (Var "A") (Not (Var "A"))`
* (A OR B) AND (NOT A) -> `And (Or (Var "A") (Var "B")) (Not (Var "A"))`

**Example CLI Session:**
```text
=== Interactive Formula Builder ===
Type your formula using strict constructors (And, Or, Not, Var, Value).
Example: And (Var "A") (Or (Not (Var "A")) (Var "B"))
(Type 'exit' to return to the Main Menu)
Enter formula: And (Var "A") (Not (Var "A"))

Analyzing: And (Var "A") (Not (Var "A"))
Result: UNSATISFIABLE

Do you want to try another formula?
1. Yes!
2. No, exit to main menu!
3. No, exit the program!
Select an option: 
```

---

## 🧪 Testing

To run the property tests and verify the DPLL engine against the pure truth-table Oracle, execute:
```bash
cabal test
```