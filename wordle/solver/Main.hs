module Main where

import Solve

main :: IO ()
main = do
  solveTheGame $ initialSolver Naive -- por defecto, pero se cambia de ser necesario en Solve
  pure ()                            -- un poco limitado por solo poder importar solveTheGame, 
                                     -- initialSolver y Solver
  
