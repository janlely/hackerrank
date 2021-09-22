{-# LANGUAGE MultiWayIf #-}
module Lib
    ( someFunc
    ) where

import Control.Monad
import Data.List as L
import Data.Array as A
import Data.IORef (newIORef, modifyIORef, readIORef)


newtype DList a = UnsafeDList {unsafeApplyDList :: [a] -> [a]}

toDList :: [a] -> DList a
toDList = UnsafeDList . (++)

appendDList :: DList a -> DList a -> DList a
appendDList xs ys = UnsafeDList $ unsafeApplyDList xs . unsafeApplyDList ys

toNormalList :: DList a -> [a]
toNormalList = ($ []) . unsafeApplyDList

singleton :: a -> DList a
singleton = UnsafeDList . (:)

emptyDList :: DList a
emptyDList = UnsafeDList id

data Tree
    = Leaf
    | TreeNode { value :: Int
           , depth :: Int
           , left :: Tree
           , right :: Tree
           } 
instance Show Tree where
    show tr = show' tr 0
        where show' Leaf n = replicate n ' ' ++ "Leaf"
              show' (TreeNode v d l r) n = replicate n ' ' ++ "value: " ++ show v ++ "\n" ++ replicate n ' ' ++ show' l (n + 2) ++ "\n" ++ replicate n ' ' ++ show' r (n+2)
    

buildTree :: A.Array Int ((Int, Int), (Int, Int)) -> (Int,Int) -> Int -> Tree
buildTree _ (_, -1) _  = Leaf
buildTree arr (v,i) d = TreeNode v d (buildTree arr (l, il) (d+1)) (buildTree arr (r, ir) (d+1))
    where ((l,il), (r,ir)) = arr A.! i

exchange :: Tree -> Int -> Tree
exchange Leaf _ = Leaf
exchange (TreeNode v d l r) k = if mod d k == 0
                                   then TreeNode v d (exchange r k) (exchange l k)
                                   else TreeNode v d (exchange l k) (exchange r k)

inorder :: Tree -> [Int]
inorder tr = toNormalList $ inorder' tr

inorder' :: Tree -> DList Int
inorder' Leaf = emptyDList
inorder' (TreeNode v _ l r) = inorder' l `appendDList` (singleton v) `appendDList` inorder' r

query :: [Int] -> Tree -> IO ()
query [] _ = return ()
query (k:ks) tr = do
    let tr' = exchange tr k
    putStrLn $ unwords (show <$> inorder tr')
    query ks tr'


someFunc = do
    n <- read <$> getLine
    idx <- newIORef 1
    childs <- replicateM n $ do
        [l,r] <- fmap read . words <$> getLine
        when (l /= -1) $ modifyIORef idx (+1)
        il <- readIORef idx
        when (r /= -1) $ modifyIORef idx (+1)
        ir <- readIORef idx
        return if | l /= -1 && r /= -1 -> ((l, il), (r, ir))
                  | l /= -1            -> ((l, il), (-1,-1))
                  | r /= -1            -> ((-1,-1), (r, ir))
                  | otherwise          -> ((-1,-1), (-1,-1))
    let tree = buildTree (A.listArray (1,n) childs) (1,1) 1
    -- putStrLn $ unwords (show <$> inorder tree)
    t <- read <$> getLine
    ks <- replicateM t (read <$> getLine)
    query ks tree

  
