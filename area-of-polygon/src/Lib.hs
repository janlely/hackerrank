module Lib
    ( someFunc
    ) where

import Data.List
import Control.Monad

calcArea :: [(Double, Double)] -> Double
calcArea ps = let (psx, psy) = unzip ps
                  a = ((last psx * head psy) +) . sum $ zipWith (*) (init psx) (tail psy)
                  b = ((head psx * last psy) +) . sum $ zipWith (*) (tail psx) (init psy)
               in 0.5 * abs (a-b)


-- 计算一个多边形的面积
-- 输入：第一行N指定后续N行输入，每一行一个点，例如 1 0。这些点是一个多边形的顶点，按逆时针方向输入。
-- 输出：这个多边形的面积
-- 提示：多边形面积的鞋带公式

someFunc = do
    pc <- (read::String->Double) `fmap` getLine
    ps <- forM [1..pc] $ const $ do
        a:b:_ <- (fmap (read::String->Double) . words) `fmap` getLine
        return (a,b)
    print $ calcArea ps

