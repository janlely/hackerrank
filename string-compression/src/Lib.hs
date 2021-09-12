module Lib
    ( someFunc
    ) where


compress :: String -> String
compress "" = ""
compress (a:as) = let (l, r) = span (== a) as
                      len = length l
                   in if len > 0
                         then a:(show (len + 1) ++ compress r)
                         else a:compress r

someFunc = do
    orignal <- getLine
    putStrLn $ compress orignal
