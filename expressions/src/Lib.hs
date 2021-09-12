module Lib
    ( someFunc
    ) where

import Data.List

findExpression :: [Int] -> Maybe String
findExpression xs = fmap snd $ find ((== 0) . fst) $ foldl' f [(head xs, "")] $ tail xs
    where f res x = nubBy (\a b -> fst a == fst b) $
                        concatMap (\(v, ops) ->
                            [((v+x) `mod` 101, '+':ops),
                             ((v-x) `mod` 101, '-':ops),
                             ((v*x) `mod` 101, '*':ops)]) res

-- 类似计算24点。给一组数添加+-*三种运算，使结果可以被101整除，忽略运算符优先级。
-- 输入：第一行N指示有多少个数，第二行N个数，以空格分隔
-- 输出：一个表达式
-- 例如：
-- 3
-- 22 79 21
-- 22*79-21
someFunc = do
    n <- readLn :: IO Int
    ns <- map read . words <$> getLine
    case findExpression ns of
      Nothing -> putStrLn "No Answer"
      Just ans -> let result = concat $ zipWith (\a b -> show a ++ [b]) (init ns) (reverse ans)
                   in putStrLn $ result ++ (show $ last ns)
