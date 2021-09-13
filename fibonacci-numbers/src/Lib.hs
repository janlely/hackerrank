module Lib
    ( someFunc
    ) where

import Data.List as L

fib 1 = 0
fib 2 = 1
fib n = snd $ L.foldl' f (0,1) [3..n]
    where f (a0, a1) _ = (a1, a0+a1)

someFunc = do
    n <- read <$> getLine
    print $ fib n
