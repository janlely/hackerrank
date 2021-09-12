{-# LANGUAGE TupleSections #-}
module Lib
    ( someFunc
    ) where

import Data.List as L
import Data.Map as M
import Data.Ord (comparing)
import Data.Maybe
import Control.Monad
import Debug.Trace


getSlots :: [String] -> [[(Int, Int)]]
getSlots grid = L.filter ((> 1) . length) $ scanLines 0 grid ++ fmap (fmap exchange) (scanLines 0 (L.transpose grid))
    where scanLines _ [] = []
          scanLines i (l:ls) = scanLine i 0 l ++ scanLines (i+1) ls
          scanLine _ _ ""  = []
          scanLine i j str = let (s, ss) = span (== '+') str
                                 jj = j + length s
                                 (s', ss') = span (== '-') ss
                                 jjj = jj + length s' - 1
                              in fmap (i,) [jj..jjj] : scanLine i (jjj+1) ss'
          exchange = uncurry (flip (,))

findResult :: [[(Int, Int)]] -> [String] -> [([(Int, Int)], String)]
findResult slots ws = fromJust $  L.find g $ doFind (f slots) (f ws)
    where f = L.groupBy (\x y -> length x == length y) . L.sortBy (comparing length)
          g result = trace ("result: " ++ show result) isJust $ L.foldl' (\mm (p, ch) -> 
              case mm of
                Just m -> let ch' = M.lookup p m
                           in if isJust ch' && fromJust ch' /= ch then Nothing else Just $ M.insert p ch m
                Nothing -> Nothing) (Just M.empty) $ concatMap (uncurry zip) result
          doFind :: [[[(Int, Int)]]] -> [[String]] -> [[([(Int, Int)], String)]]
          doFind [] [] = [[]]
          doFind (a1:as) (b1:bs) = do
              sw <- ((zip a1) <$> L.permutations b1)
              sws <- doFind as bs
              return $ sw ++ sws

someFunc = do
    grid <- sequence $ const getLine <$> [1..10] 
    wordsLine <- getLine
    let ws = words $ (\c -> if c == ';' then ' ' else c) <$> wordsLine
        slots = getSlots grid
        m = M.fromList $ concatMap (uncurry zip) $ findResult slots ws
    print $ slots
    forM [0..9] (\i -> do
        forM [0..9] (\j -> do
            case M.lookup (i,j) m of
              Just c -> putChar c
              Nothing -> putChar '+')
        putChar '\n')

