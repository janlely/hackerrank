module Lib
    ( solve
    ) where

import Control.Monad
import Data.List (intersperse)

makeup :: (Int, Int) -> [[Char]]
makeup (h,w) = build (w `div` 2)
    where build 0 = [replicate w '1']
          build n = (replicate n '_' ++ replicate (w-2*n) '1' ++ replicate n '_') : build (n-1)

makeup' :: [[Char]] -> [[Char]]
makeup' xs = let w = length (head xs)
                 h = length xs
                 n = (w+1) `div` 2
                 up = (\x -> replicate n '_' ++ x ++ replicate n '_') <$> xs
                 down = (\x -> x ++ ('_':x)) <$> xs
              in up ++ down



breakup :: (Int, Int) -> (Int, Int)
breakup (h,w) = (h `div` 2, w `div` 2)

applyNTimes :: Int -> (a -> a) -> a -> a
applyNTimes 0 _ a = a
applyNTimes 1 f a = f a
applyNTimes n f a = f $ applyNTimes (n-1) f a

iterateN :: Int -> [[Char]]
iterateN n = let tile = applyNTimes n breakup (32, 63)
              in applyNTimes n makeup' (makeup tile)

solve = do
    n <- read <$> getLine
    forM (iterateN n) (\x -> putStrLn x)

