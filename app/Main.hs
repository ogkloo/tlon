module Main where

import Tlon

main :: IO ()
main = putStrLn (renderGameState defaultGameState)
