module Lib
    ( someFunc
    ) where


import Data.List as L
import Data.Maybe
import Control.Monad
import Debug.Trace

nsum :: Int -> [Int] -> [Maybe [Int]]
nsum t [] = []
nsum t (x:xs) = f (t-x) [x] xs ++ nsum t xs
    where f 0 hs _     = [Just hs]
          f _ _ []     = [Nothing]
          f t' hs xs' = if t' > 0
                           then g hs <$> nsum t' xs'
                           else [Nothing]
          g hs (Just ts) = Just $ hs ++ ts 
          g hs Nothing = Nothing

solve :: Int -> Int -> Int
solve x n = length (catMaybes $ nsum x xs)
    where xs = L.takeWhile (<= x) $ (^ n) <$> [1..]

someFunc = do
    x <- read <$> getLine
    n <- read <$> getLine
    print $ solve x n
