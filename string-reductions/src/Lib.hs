module Lib
    ( someFunc
    ) where

import Data.Set as S
import Data.List as L

someFunc = do
    str <- getLine
    putStrLn $ (reverse . snd) (L.foldl' f (S.empty, []) str)
  where f (s, hs) ch = if S.member ch s then (s, hs) else (S.insert ch s, ch:hs)
