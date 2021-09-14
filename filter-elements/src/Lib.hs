module Lib
    ( someFunc
    ) where

import Data.Map.Strict as M
import Data.List as L
import Control.Monad


filterElem :: [Int] -> Int -> [Int]
filterElem xs k = L.filter (flip M.member (M.filter (>= k) m)) $ L.nub xs
    where m = L.foldl' (\mp elem -> M.insertWith (+) elem 1 mp) M.empty xs

someFunc = do
    n <- read <$> getLine
    forM [1..n] $ const $ do
        [n', k] <- (fmap read . words) <$> getLine
        xs <- (fmap read . words) <$> getLine
        case filterElem xs k of
          [] -> putStrLn "-1"
          xs' -> putStrLn $ unwords $ fmap show xs'
        
