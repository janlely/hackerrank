module Lib
    ( someFunc
    ) where


import Control.Monad
-- import Debug.Trace


data SegTree = SegNode {value :: Integer, section :: (Int, Int), left :: SegTree, right :: SegTree} | SegLeaf {value :: Integer, end :: Int}

-- instance Show SegTree where
    -- show tr = show' tr 0
        -- where show' (SegLeaf v e) n = replicate n ' ' ++ "value: " ++ show v ++ "\n" ++ replicate n ' ' ++ "end: " ++ show e
              -- show' (SegNode v s l r) n = replicate n ' ' ++ "value: " ++ show v ++ "\n" ++ replicate n ' ' ++ "section: " ++ show s ++ "\n" ++ show' l (n+4) ++ "\n" ++ show' r (n+4)

emptySegTree :: Int -> Int -> SegTree 
emptySegTree i j
  | i == j = SegLeaf 0 i
  | otherwise = SegNode 0 (i,j) (emptySegTree i mi) (emptySegTree (mi+1) j)
  where mi = div (i + j) 2

updateSegTree :: (Integer -> Integer -> Integer) -> SegTree -> (Int, Integer) -> SegTree
updateSegTree f (SegLeaf v' _) (i,v)  = SegLeaf (f v' v) i
-- updateSegTree f (SegLeaf v' _) (i,v)  = SegLeaf (f v' v `mod` 1000000007) i
updateSegTree f (SegNode _ (m, n) l r) (i,v)
  | i <= mi   = (SegNode (lcm (value l') vr) (m, n) l' r)
  | otherwise = (SegNode (lcm (value r') vl) (m, n) l r')
  where mi = div (m + n) 2
        vl = value l
        vr = value r
        l' = updateSegTree f l (i, v)
        r' = updateSegTree f r (i, v)

findLCM :: SegTree -> Int -> Int -> Integer
findLCM (SegLeaf v _) _ _ = v
findLCM (SegNode v (i,j) l r) a b
  | i == a && j == b = v
  | a > mi           = findLCM r a b
  | b <= mi          = findLCM l a b
  | otherwise        = lcm (findLCM l a mi) (findLCM r (mi+1) b)
  where mi = div (i + j) 2

someFunc = do
    n <- read <$> getLine
    xs <- fmap read . words <$> getLine
    q <- read <$> getLine
    let segTree = foldl (updateSegTree (flip const)) (emptySegTree 0 (n-1)) $ zip [0,1..] xs
        query :: [(Char, Int, Integer)] -> SegTree -> IO ()
        query [] _ = return ()
        query (('Q', l, r):qs) tr = do
            -- trace ("segTree: " ++ show tr) print $ findLCM tr l r `mod` 1000000007
            print $ findLCM tr l (fromInteger r) `mod` 1000000007
            query qs tr
        query (('U', i, v):qs) tr = query qs (updateSegTree (*) tr (i, v))
    qs <- replicateM q $ do
        [c, a, b] <- words <$> getLine
        return (head c, (read::String->Int) a, (read::String->Integer) b)
    query qs segTree
            
            

    
