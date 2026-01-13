Project Overview
This project is a Sudoku solver developed in DrRacket to explore functional programming and algorithmic efficiency. It currently successfully solves puzzles using a recursive approach.

Technical Implementation
The solver is built on the following foundations:

Backtracking Algorithm: Uses a depth-first search strategy to explore potential solutions and revert when constraints are violated.

Functional Programming: Leverages Racket’s immutable data structures and recursion to manage the puzzle state.

MRV (Minimum Remaining Value) Heuristic (In-Progress): I am currently refining the MRV logic to optimize the search space. 
While the core backtracking logic is functional, implementing full heuristic pruning is the next milestone to enable the solver to handle Hard/Expert puzzles within standard time complexity limits.
