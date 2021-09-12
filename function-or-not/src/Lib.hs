module Lib
    ( someFunc
    ) where

import Control.Monad
import qualified Data.Map as M

isValid :: [(Int, Int)] -> Bool
isValid pairs = and . snd  $ foldr g (M.empty, []) pairs
    where g (a, b) (m, bs) = case M.lookup a m of
                                Just v -> if v == b then (m, True:bs) else (m, False:bs)
                                Nothing -> (M.insert a b m, True:bs)


-- 输入: 第一行为数字N,指示接下来有N行输入,每一行一个数对,例如 1 1
-- 输出：如果这些数对可能是某一个函数的输入和输出值则打印YES, 否则打印NO
-- 提示: 函数的特殊是，同个输入必然对应同一个输出。
someFunc = do
    cases <- (read::String->Int) `fmap` getLine
    forM [1..cases] $ const $ do
        lines <- (read::String->Int) `fmap` getLine
        pairs <- forM [1..lines] $ const $ do
            a:b:_ <- (fmap (read::String->Int) . words) `fmap` getLine
            return (a,b)
        putStrLn $ if isValid pairs then "YES" else "NO" 
            

        

