module Lib
    ( someFunc
    ) where

import Control.Monad
import Data.List as L

judge :: String -> Bool
judge = judge' . L.foldl' f (0,0,0,0,True)
  where f (_,_,_,_, False) _ = (0,0,0,0, False)
        f (a,b,c,d,_) ch = case ch of
                            'R' -> if a >= 1 then (0,0,0,0,False) else (a+1,b-1,c,d,True)
                            'G' -> if b >= 1 then (0,0,0,0,False) else (a-1,b+1,c,d,True)
                            'Y' -> if c >= 1 then (0,0,0,0,False) else (a,b,c+1,d-1,True)
                            'B' -> if d >= 1 then (0,0,0,0,False) else (a,b,c-1,d+1,True)
        judge' (0,0,0,0,True) = True
        judge' _ = False


someFunc = do
  n <- read <$> getLine
  forM [1..n] $ const $ do
    str <- getLine
    print $ judge str
