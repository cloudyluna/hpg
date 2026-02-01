module Main (main) where

import qualified ProjectGenerator (someFunc)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  ProjectGenerator.someFunc
