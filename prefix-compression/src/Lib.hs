module Lib
    ( someFunc
    ) where


import Control.Monad

prefixCount :: String -> String -> Int
prefixCount [] _ = 0
prefixCount _ [] = 0
prefixCount (a:as) (b:bs) = if a == b then 1 + prefixCount as bs else 0

someFunc = do
    str1 <- getLine
    str2 <- getLine
    let i = prefixCount str1 str2
    putStr $ show i
    putStrLn $ " " ++ take i str1
    putStr $ show (length str1 - i)
    when (length str1 - i > 0) $ putStr $ " " ++ drop i str1
    putStrLn ""
    putStr $ show (length str2 - i)
    when (length str2 - i > 0) $ putStr $ " " ++ drop i str2
    putStrLn ""
    
