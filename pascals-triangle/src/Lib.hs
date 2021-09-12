module Lib
    ( solve
    ) where

import Control.Monad 

generate :: Int -> [[Int]]
generate 0 = []
generate 1 = [[1]]
generate 2 = [[1,1],[1]]
generate n = let a = generate (n-1)
              in f (head a):a
             where f xs = 1:(uncurry (+) <$> zip (init xs) (drop 1 $ cycle xs)) ++ [1]


solve = do
    n <- read <$> getLine
    forM (reverse $ generate n) (\i -> putStrLn $ unwords $ show <$> i)
