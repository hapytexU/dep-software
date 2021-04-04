module Main where

import Brick(simpleMain)
import Dep.Bricks.Circuit(circuit)

main :: IO ()
main = simpleMain circuit
