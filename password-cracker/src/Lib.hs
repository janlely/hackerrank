module Lib
    ( someFunc
    ) where


import Control.Monad
import Data.Array
import Data.Maybe

isGood :: Array Int Char -> Int -> String -> Bool
isGood _ _  "" = True
isGood arra i (x:xs) = i <= snd (bounds arra) && arra ! i == x && isGood arra (i+1) xs

findCombination :: String -> [String] -> Maybe [String]
findCombination target sources =
    let n = length target
        arra = listArray (1, n) target
        dp = listArray (1, n+1) (map f [1..n] ++ [Just []])
        f idx = listToMaybe $ mapMaybe try $ filter (isGood arra idx) sources
            where try pre = case dp ! (idx + length pre) of
                                Nothing -> Nothing
                                Just rs -> Just (pre:rs)
     in dp ! 1

someFunc = do 
    t <- readLn
    forM_ [1..t] (\_ -> do n <- readLn :: IO Int
                           sources <- fmap words getLine
                           target <- getLine
                           let output = case (findCombination target sources) of
                                 Nothing  -> "WRONG PASSWORD"
                                 Just xs -> unwords xs
                           putStrLn output)



