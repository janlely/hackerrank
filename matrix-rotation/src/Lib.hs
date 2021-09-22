{-# LANGUAGE TupleSections #-}
module Lib
    ( someFunc
    ) where

import Data.Array as A
import Data.List as L
import Control.Monad
-- import Debug.Trace

rotate :: Int -> A.Array (Int,Int) Int -> A.Array (Int, Int) Int
rotate r arr = let cls = cycles m n 
                   rs = (mod r) . cycleLength <$> cls
                in doRotate rs cls 1 arr
    
    where (m,n) = snd $ A.bounds arr

          cycles :: Int -> Int -> [(Int, Int)]
          cycles m n
            | m > 2 && n > 2 = (m,n): cycles (m-2) (n-2)
            | otherwise      = [(m,n)]

          cycleLength :: (Int, Int) -> Int
          cycleLength (m,n) = 2 * (m + n) - 4

          doRotate :: [Int] -> [(Int, Int)] -> Int -> A.Array (Int, Int) Int -> A.Array (Int, Int) Int
          doRotate [] [] _ arr = arr
          doRotate (i:is) ((m,n):mns) d arr = doRotate is mns (d+1) $ doRotate' i (m,n) d arr

          doRotate' :: Int -> (Int, Int) -> Int -> A.Array (Int, Int) Int -> A.Array (Int, Int) Int 
          doRotate' i (m,n) d arr = let idx = indexes d m n
                                        len = length idx
                                        vs = (arr A.!) <$> idx
                                     in arr A.// (zip idx (take len $ drop i $ cycle vs))

          indexes :: Int -> Int -> Int -> [(Int, Int)]
          indexes d m n = let u = (d,) <$> [d,d+1..d+n-1]
                              r = (,d+n-1) <$> [d,d+1..d+m-1]
                              b = (d+m-1,) <$> [d+n-1,d+n-2..d]
                              l = (,d) <$> [d+m-1,d+m-2..d]
                           in ((tail u ++) . (tail r ++) . (tail b ++) . (tail l ++)) []
                           -- in trace ("l: " ++ show l ++ "b: " ++ show b ++ "r: " ++ show r ++ "u: " ++ show u) ((tail u ++) . (tail r ++) . (tail b ++) . (tail l ++)) []
              


printMatrix :: Int -> A.Array (Int, Int) Int -> String
printMatrix n arr = L.intercalate "\n" $ (unwords . fmap show . fmap snd) <$> L.groupBy gf (A.assocs arr) 
    where gf ((i,_),_) ((j,_),_) = i == j
          

someFunc = do
    [m,n,r] <- fmap read . words <$> getLine
    vs <- fmap concat (replicateM m (fmap read . words <$> getLine))
    let matrix = A.listArray ((1,1), (m,n)) vs
    putStrLn $ printMatrix n $ rotate r matrix
