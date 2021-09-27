{-# LANGUAGE BangPatterns#-}
module Lib
    ( someFunc
    ) where

import Data.Array as A
import Control.Monad

data SegTree
    = Empty
    | SegNode {_value :: Int
              , _left :: SegTree
              , _right :: SegTree
              , _section :: (Int, Int)} deriving Show

buildSegTree :: A.Array Int Int -> (Int, Int) -> SegTree
buildSegTree arr (i,j)
  | i == j = SegNode (arr A.! i) Empty Empty (i,i)
  | otherwise = let l = buildSegTree arr (i, mi)
                    r = buildSegTree arr (mi+1, j)
                 in SegNode (min (_value l) (_value r)) l r (i,j)
  where mi = div (i+j) 2

findMin :: SegTree -> (Int, Int) -> Int
findMin (SegNode v l r (i,j)) (i',j')
  | (i,j) == (i',j') = v
  | i' > mi = findMin r (i',j')
  | j' <= mi = findMin l (i',j')
  | otherwise = min (findMin l (i', mi)) (findMin r (mi+1, j'))
  where mi = div (i+j) 2

someFunc = do
    [n,q] <- fmap read . words <$> getLine
    ns <- fmap read . words <$> getLine
    let !segTree = buildSegTree (A.listArray (0,n-1) ns) (0,n-1)
    replicateM q $ do
        [i,j] <- fmap read . words <$> getLine
        print $ findMin segTree (i,j)
        
