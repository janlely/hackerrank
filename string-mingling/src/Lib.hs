module Lib
    ( someFunc
    ) where

someFunc = do
    str1 <- getLine
    str2 <- getLine
    putStrLn $ concatMap (\(x,y) -> [x,y]) $zip str1 str2
