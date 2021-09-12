module Lib
    ( someFunc
    ) where

import Data.List
import Control.Monad

-- 计算多边形财长
-- 输入：第一行N指示多少个点，接下来N行，每行一个点，将这些点连接可以组成一个多边形
-- 输出：财长
-- 示例：
-- 4
-- 0 0
-- 0 1
-- 1 1
-- 1 0
--
-- 4
someFunc = do
    pc <- (read::String->Double) `fmap` getLine
    ps <- forM [1..pc] $ const $ do
        a:b:_ <- (fmap (read::String->Double) . words) `fmap` getLine
        return (a,b)
    print $ snd $ foldl f (last ps, 0) ps
  where f ((x0,y0), s) (x1,y1) = ((x1,y1), s + (sqrt $ (x1-x0)^2 + (y1-y0)^2))

