module Lib
    ( someFunc
    ) where

import Data.Array as A
import Data.List as L
import Data.Bifunctor (first, second)


-- 动态规划
calcLeft :: Int -> A.Array Int Int -> A.Array Int Int
calcLeft n fs = lefti
  where lefti = A.listArray (1,n) (1:fmap f [2..n])
        f i = f' i (i-1)
        f' i j
          | j == 0 || fs A.! j < fs A.! i = j+1
          | otherwise = f' i ((lefti A.! j) - 1)

-- 栈
calcLeft' :: Int -> A.Array Int Int -> A.Array Int Int
calcLeft' n arr = A.listArray (1,n) $ (snd $ L.foldl f ([],id) [1..n]) []
    where f ([],ps) i = ([i], ps . (1:))
          f (q:qs, ps) i = if arr A.! q < arr A.! i
                              then (i:q:qs, ps . ((q+1):))
                              else f (qs, ps) i

calcMax :: A.Array Int Int -> A.Array Int Int -> A.Array Int Int -> Int
calcMax src lefti righti = (L.maximum . fmap f) $ A.indices src
    where f i = (src A.! i) * ((righti A.! i) - (lefti A.! i) + 1)

someFunc = do
    n <- read <$> getLine
    fs <- fmap read . words <$> getLine
    let lefti = calcLeft n $ A.listArray (1,n) fs
        righti = A.array (1,n) $ fmap (first (7-) . second (7-)) $ A.assocs $ calcLeft n $ A.listArray (1,n) (reverse fs)
    print $ calcMax (A.listArray (1,n) fs) lefti righti
