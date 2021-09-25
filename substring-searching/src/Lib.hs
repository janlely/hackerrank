module Lib
    ( someFunc
    ) where

import Control.Monad
import Data.Array as A
import Data.List as L

kmpSearch :: A.Array Int Char -> A.Array Int Char -> A.Array Int Int -> Bool
kmpSearch src dest next = kmpSearch' next src lb dest lb'
    where (lb, rb)   = A.bounds src
          (lb', rb') = A.bounds dest
          len = rb' - lb' + 1 
          kmpSearch' next src i dest j
            | j > rb'                 = True
            | i > rb                  = False
            | src A.! i == dest A.! j = kmpSearch' next src (i+1) dest (j+1)
            | j == lb'                = kmpSearch' next src (i+1) dest j
            | otherwise               = kmpSearch' next src i dest (lb' + next A.! (j-1))

nextI :: A.Array Int Char -> A.Array Int Int
nextI src = init A.// (L.foldl' f [(lb,0)] [lb+1..rb])
  where (lb, rb) = A.bounds src
        init = A.listArray (lb, rb) (repeat 0)
        f res@((_,n):_) i = if (src A.! (lb+n)) == (src A.! i) then (i,n+1):res else (i,0):res


someFunc = do
    n <- read <$> getLine
    replicateM n $ do
        str1 <- getLine
        str2 <- getLine
        let next = nextI $ A.listArray (1, length str2) str2
        if kmpSearch (A.listArray (1, length str1) str1) (A.listArray (1, length str2) str2) next
           then putStrLn "YES"
           else putStrLn "NO"

