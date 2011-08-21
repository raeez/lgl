module Main where
import Criterion.Main
import Dense

main :: IO ()
main = defaultMain [
    bgroup "scc" $ denseSuite
    ]
