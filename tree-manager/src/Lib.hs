module Lib
    ( someFunc
    ) where

import Control.Monad

data Tree a = Empty | Node a [Tree a] deriving (Show) 
data Crumb a = Crumb a [Tree a] [Tree a] deriving (Show)
type Breadcrumbs a = [Crumb a] 
type Zipper a = (Tree a, Breadcrumbs a)

changeValue :: Zipper a -> a -> Zipper a
changeValue (Node _ cs, bs) x = (Node x cs, bs)

printValue :: (Show a) => Zipper a -> a
printValue (Node a _, _) = a

visitLeft :: Zipper a -> Zipper a
visitLeft (cur, (Crumb b l r):bs) = let lv = last l
                                        iv = init l
                                     in (lv, (Crumb b iv (cur:r)):bs)

visitRight :: Zipper a -> Zipper a
visitRight (cur, (Crumb b l r):bs) = let rh = head r
                                         rt = tail r 
                                      in (rh, (Crumb b (l ++ [cur]) rt):bs)

visitParent :: Zipper a -> Zipper a
visitParent (cur, (Crumb b l r):bs) = (Node b (l ++ [cur] ++ r), bs)

visitChild :: Zipper a -> Int -> Zipper a
visitChild (Node a cs, bs) n = let (l,r)  = splitAt (n-1) cs
                                in (head r, (Crumb a l (tail r)):bs)

insertLeft :: Zipper a -> a -> Zipper a
insertLeft (cur, (Crumb b l r):bs) x = (cur, (Crumb b (l ++ [Node x []]) r):bs)

insertRight :: Zipper a -> a -> Zipper a
insertRight (cur, (Crumb b l r):bs) x = (cur, (Crumb b l (Node x []:r):bs))

insertChild :: Zipper a -> a -> Zipper a
insertChild (Node v cs, bs) x = (Node v ((Node x []):cs), bs)

delete :: Zipper a -> Zipper a
delete (cur, []) = (Empty, [])
delete (cur, (Crumb a l r):bs) = (Node a (l++r), bs)

someFunc = do
    n <- read <$> getLine
    let tr = (Node "0" [], [])
    inputs <- replicateM n $ words <$> getLine
    let query :: [[String]] -> Zipper String -> IO ()
        query [] _ = return ()
        query (("change":v:[]):is) tr = query is (changeValue tr v)
        query (("print":[]):is) tr = putStrLn (printValue tr) >> query is tr 
        query (("visit":"left":[]):is) tr = query is (visitLeft tr)
        query (("visit":"right":[]):is) tr = query is (visitRight tr)
        query (("visit":"parent":[]):is) tr = query is (visitParent tr)
        query (("visit":"child":n:[]):is) tr = query is (visitChild tr (read n))
        query (("insert":"left":x:[]):is) tr = query is (insertLeft tr x)
        query (("insert":"right":x:[]):is) tr = query is (insertRight tr x)
        query (("insert":"child":x:[]):is) tr = query is (insertChild tr x)
        query (("delete":[]):is) tr = query is (delete tr)
    query inputs tr
