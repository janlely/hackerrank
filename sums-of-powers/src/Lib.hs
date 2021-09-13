module Lib
    ( someFunc
    ) where


import Data.List as L
import Data.Maybe
import Control.Monad
import Debug.Trace

nsum :: Int -> Int -> [Int] -> [Maybe [Int]]
nsum n t [] = []
nsum n t (x:xs) = f (n-1) (t-x) [x] xs ++ nsum n t xs
    where f 0 0 hs _     = [Just hs]
          f 0 _ _ _      = [Nothing]
          f _ _ _ []     = [Nothing]
          f n' t' hs xs' = g hs <$> nsum n' t' xs'
          g hs (Just ts) = Just $ hs ++ ts 
          g hs Nothing = Nothing

solve :: Int -> Int -> Int
solve x n = foldl' f 0 [1..m]
    where xs = L.takeWhile (<= x) $ (^ n) <$> [1..]
          m = length $ fromJust $ L.find ((>= x) . sum) $ L.inits xs
          f ans i = ans + length (catMaybes $ nsum i x xs)

someFunc = do
    x <- read <$> getLine
    n <- read <$> getLine
    print $ solve x n
