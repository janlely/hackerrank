module Lib
    ( someFunc
    ) where

import Data.Char (digitToInt)
import Control.Monad

superDigit :: String -> Int -> String
superDigit xs i = super $ show $ (calc xs) * i
    where super [c] = [c]
          super cs = super $ show $ calc cs
          calc [c] = digitToInt c
          calc (c:cs) = digitToInt c + calc cs

someFunc = do
    [n,k] <- words <$> getLine
    putStrLn $ superDigit n $ read k
    
