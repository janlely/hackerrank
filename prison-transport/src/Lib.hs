module Lib
    ( someFunc
    ) where


import Data.Graph as G
import Data.Tree as T
import Control.Monad


cost :: T.Forest Int -> Int
cost [] = 0
cost (t:ts) = cost' t + cost ts
    where cost' :: Tree a -> Int
          cost' t = calcCost 1 $ length $ T.flatten t
          calcCost :: Int -> Int -> Int
          calcCost i n
            | i * i >= n = i
            | otherwise = calcCost (i+1) n

someFunc = do
    n <- read <$> getLine
    m <- read <$> getLine
    edges <- replicateM m $ do
        [l,r] <- fmap read . words <$> getLine
        return (l,r)
    print $ cost (G.components $ G.buildG (1,n) edges)
    
    
