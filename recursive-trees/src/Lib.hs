module Lib
    ( someFunc
    ) where


import Control.Monad 


treeN :: Int -> [[Char]]
treeN n = let branch = foldl (\xs i -> (replicate (n-i) '_' ++ ('1':replicate (2*i-1) '_') ++ "1" ++ replicate (n-i) '_'):xs) [] [1..n]
              trunk = replicate n (replicate n '_' ++ "1" ++ replicate n '_')
           in branch ++ trunk

treeN' :: Int -> Int -> ([[Char]],Int)
treeN' n 0 = (treeN n,0)
treeN' n i = let n' = n `div` 2
                 (up,j) = treeN' n' (i-1)
                 up' = (\xs -> xs ++ replicate (2*n'-2*j-1) '_' ++ xs) <$> up
                 down = treeN n
                 m = (length (head up') - length (head down)) `div` 2
                 down' = (\xs -> replicate m '_' ++ xs ++ replicate m '_') <$> down
              in (up' ++ down', m)

someFunc = do
    i <- read <$> getLine
    let res = (\x -> let n = (100 - length x) `div` 2 in replicate n '_' ++ x ++ replicate n '_') <$> (fst $ treeN' 16 (i-1)) 
        res' = (\x -> let n = 100 - length x in x ++ replicate n '_') <$> res
        len = 63 - length res'
        up = replicate len $ replicate 100 '_'
    forM (up ++ res') putStrLn
