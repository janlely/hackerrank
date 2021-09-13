module Lib
    ( someFunc
    ) where


gcd' :: Int -> Int -> Int
gcd' a 0 = a
gcd' a b = let c = a `mod` b
           in gcd' b c

someFunc = do
    [a,b] <- fmap read . words <$> getLine
    print $ gcd' a b
