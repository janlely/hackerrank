{-# LANGUAGE BangPatterns#-}
module Lib
    ( someFunc
    ) where


import Data.Array as A
import Control.Monad
-- import Debug.Trace
import Data.Maybe
data SegTree
    = Empty
    | SegNode {_value :: (Int, Int)
              , _section :: (Int, Int)
              , _left :: SegTree
              , _right :: SegTree}

-- instance Show SegTree where
    -- show st = show' st 0
        -- where show' Empty _ = ""
              -- show' (SegNode v s l r) n = replicate n ' ' ++ "value: " ++ show v ++ "\n" ++ replicate n ' ' ++ "section: " ++ show s ++ "\n" ++ show' l (n+4) ++ "\n" ++ show' r (n+4)

buildSegTree :: A.Array Int Int -> Int -> Int -> SegTree
buildSegTree arr i j
  | i == j = SegNode (arr A.! i, arr A.! i) (i,i) Empty Empty
  | otherwise = let l = buildSegTree arr i mi
                    r = buildSegTree arr (mi+1) j
                 in SegNode (merge (_value l) (_value r)) (i,j) l r
  where mi = div (i+j) 2
        merge (a,b) (c,d) = (min a c, max b d)

searchLeft :: SegTree -> Int -> Int -> (Int,Int) -> Maybe Int
searchLeft st i j (vl,vr)
  | i > j = Nothing 
  | mvr <= vr && mvl >= vl = Just i
  | otherwise = let (mvl', mvr') = findMax st mi j
                 in if mvr' <= vr && mvl' >= vl
                       then Just $ fromMaybe mi $ searchLeft st i (mi-1) (vl,vr)
                       else if mi == j then Nothing else searchLeft st mi j (vl, vr)
  where (mvl, mvr) = findMax st i j
        mi = div (i + j + 1) 2

searchRight :: SegTree -> Int -> Int -> (Int,Int) -> Maybe Int
searchRight st i j (vl,vr)
  | i > j = Nothing
  | mvr <= vr && mvl >= vl = Just j
  | otherwise = let (mvl', mvr') = findMax st i mi
                 in if mvr' <= vr && mvl' >= vl
                       then Just $ fromMaybe mi $ searchRight st (mi+1) j (vl,vr)
                       else if i == mi then Nothing else searchRight st i mi (vl, vr)
  where (mvl, mvr) = findMax st i j
        mi = div (i + j) 2

findMax :: SegTree -> Int -> Int -> (Int, Int)
findMax st@(SegNode (a,b) (sa, sb) l r) i j
  | i == sa && j == sb = (a,b)
  | j <= mi = findMax l i j
  | i > mi = findMax r i j
  | otherwise = merge (findMax l i mi) (findMax r (mi+1) j)
  where mi = div (sa+sb) 2
        merge (a,b) (c,d) = (min a c, max b d)

query :: SegTree -> A.Array Int Int -> Int -> Int -> Int
query st arr d m = fromMaybe d ri - fromMaybe d li + 1 
    where li = if d > lb then searchLeft st lb (d-1) (dv, dv+m) else Just d
          ri = if d < rb then searchRight st (d+1) rb (dv, dv+m) else Just d
          dv = arr A.! d
          (lb,rb) = A.bounds arr

-- query :: A.Array Int Int -> Int -> Int -> Int
-- query arr d m = ri - li + 1
    -- where ad = arr A.! d
          -- li = searchLeft (d-1)
          -- ri = searchRight (d+1)
          -- (lb, rb) = A.bounds arr
          -- searchLeft :: Int -> Int
          -- searchLeft res
            -- | res < lb = lb
            -- | v < ad || v > (ad+m) = res + 1
            -- | otherwise = searchLeft (res-1)
            -- where v = arr A.! res
          -- searchRight :: Int -> Int
          -- searchRight res
            -- | res > rb = rb
            -- | v < ad || v > (ad+m) = res - 1
            -- | otherwise = searchRight (res+1)
            -- where v = arr A.! res

someFunc = do
    n <- read <$> getLine
    ps <- fmap read . words <$> getLine
    q <- read <$> getLine
    let !arr = A.listArray (0,n-1) ps
        !st = buildSegTree arr 0 (n-1)
    -- print st
    replicateM q $ do
        [d,m] <- fmap read . words <$> getLine
        print $ query st arr d m
    

