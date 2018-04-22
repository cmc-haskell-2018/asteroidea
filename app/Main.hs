{-|
Module      : Main
Description : Main module, running script.
Copyright   : Just Nothing
Stability   : Stable
-}
module Main where
import System.Random (newStdGen, next)
import qualified System.Environment as S
import RunIO

main :: IO ()
main = do
  genRand <-  newStdGen
  let (seed, _) = next genRand
  commandArgs <- S.getArgs
  parseArgs commandArgs seed
  putStrLn "For the Great Good!"
