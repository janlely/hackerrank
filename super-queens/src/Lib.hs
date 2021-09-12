module Lib
    ( someFunc
    ) where


queenN :: Int -> Int -> [[(Int, Int)]]
queenN n 0 = [[]]
queenN n i = [(i, j):ps|j <- [1..n], ps <- queenN n (i-1), safe ps (i, j)]
    where safe [] _ = True
          safe ps (x, y) = all (f (x,y)) ps
          f (x1,y1) (x2,y2) = let diffx = abs (x1 - x2)
                                  diffy = abs (y1 - y2)
                                  a = if diffx == 2 then diffy /= 1 else True
                                  b = if diffy == 2 then diffx /= 1 else True
                               in diffx /= diffy && a && b && y1 /= y2

someFunc = do
    n <- read <$> getLine
    let result = queenN n n
    print $ length result
