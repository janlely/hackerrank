module Lib
    ( someFunc
    ) where


import Control.Monad

someFunc = do
    n <- read <$> getLine
    forM [1..n] $ const $ do
        str <- getLine
        let (a,b,_) = foldr (\e (x,y,z) -> if z then (e:x, y, not z) else (x, e:y, not z)) ([],[], True) str
        putStrLn $ concatMap (\(x,y) -> [x,y]) $ zip a b
