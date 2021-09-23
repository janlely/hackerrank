module Lib
    ( someFunc
    ) where


import Control.Monad
import Data.List as L
import Data.Bifunctor (second)

validBST :: [Int] -> Bool
validBST [] = True
validBST (e:es)
  | tails /= [] = False 
  | otherwise   = validBST left && validBST right
  where (left, (right, tails)) = (second (L.span (> e)) . (L.span (< e))) es

someFunc = do
    t <- read <$> getLine
    replicateM t $ do
        _ <- getLine
        ns <- fmap read . words <$> getLine
        if validBST ns
           then putStrLn "YES"
           else putStrLn "NO"



