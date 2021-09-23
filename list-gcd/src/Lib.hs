module Lib
    ( someFunc
    ) where


import Control.Monad
import Data.List as L


listGCD :: [[Int]] -> [Int]
listGCD es = L.foldl1 f es
    where f a b = listGCD' a b []

listGCD' :: [Int] -> [Int] -> ([Int] -> [Int])
listGCD' [] _ = id
listGCD' _ [] = id
listGCD' a@(p:n:pns) b@(p':n':pns')
  | p == p' = ([p, min n n'] ++) . listGCD' pns pns'
  | p < p' = listGCD' pns b
  | p > p' = listGCD' a pns'

someFunc = do
    n <- read <$> getLine
    es <- replicateM n $ fmap read . words <$> getLine
    putStrLn $ unwords $ show <$> listGCD es
        

