module Lib
    ( someFunc
    ) where


import Data.Map.Strict as M
import Data.Maybe
import Data.List as L
import qualified Control.Monad.State.Strict as SS
import Control.Monad.IO.Class (liftIO)
import Control.Monad
import Debug.Trace
import Data.Bifunctor (first)
import qualified Data.Graph as G
import Data.Ord (comparing)
import Data.DList as D

import Data.Array as A

data EmpTree = EmpNode Int Int [EmpTree] deriving Show

data SegTree = SegNode {
                 left :: SegTree,
                 right :: SegTree,
                 value :: Int,
                 section :: (Int, Int) }
            |  SegLeaf {
                 value :: Int,
                 end :: Int} deriving Show

emptySegTree :: Int -> Int -> SegTree
emptySegTree i j = if i == j
                      then SegLeaf 0 j
                      else SegNode l r 0 (i,j)
  where l = emptySegTree i mi
        r = emptySegTree (mi+1) j
        mi = div (i+j) 2

updateSegTree :: Int -> SegTree -> SegTree
updateSegTree _ (SegLeaf v e) = SegLeaf (v+1) e
updateSegTree i (SegNode l r v (a,b))
    | i <= mi   = SegNode (updateSegTree i l) r (v+1) (a,b)
    | otherwise = SegNode l (updateSegTree i r) (v+1) (a,b)
    where mi = div (a+b) 2

buildEmpTree :: G.Graph -> Int -> EmpTree 
buildEmpTree g u = EmpNode u (childCount + 1) childs
    where childs = buildEmpTree g <$> (g A.! u)
          childCount = sum $ (\(EmpNode _ c _) -> c) <$> childs


preorderTraversal :: EmpTree -> [(Int, Int)]
preorderTraversal = D.toList . preorder
    where preorder (EmpNode u n cs) = D.cons (u,n) $ concatMapD preorder (D.fromList cs)
          concatMapD f = D.foldr D.append D.empty . D.map f


-- preorderTraversal :: EmpTree -> [(Int, Int)]
-- preorderTraversal tr = preorder tr []
    -- where preorder (EmpNode u n []) = ((u,n):)
          -- preorder (EmpNode u n cs) = L.foldl (.) ((u,n):) $ preorder <$> cs

findKthEmployee :: A.Array Int SegTree -> Int -> Int -> Int -> Int
findKthEmployee segTrs start end k = doFind startTr endTr k
    where startTr = segTrs A.! start
          endTr = segTrs A.! end
          doFind (SegLeaf _ _) (SegLeaf _ e) _ = e
          doFind segTr1 segTr2 k
            | k' >= k        = doFind (left segTr1) (left segTr2) k
            | otherwise      = doFind (right segTr1) (right segTr2) (k - k')
            where lv1 = value $ left segTr1
                  lv2 = value $ left segTr2
                  k' = lv2 - lv1

query :: Int -> A.Array Int SegTree -> A.Array Int (Int, Int) -> A.Array Int Int -> A.Array Int Int -> SS.StateT Int IO [()]
query q segTrs pre preIdx ss = do
    replicateM q $ do
        [v,k] <- liftIO $ fmap read . words <$> getLine
        d <- SS.get
        let from = preIdx A.! (d+v)
            to = from + (snd $ pre A.! from) - 1
            p = findKthEmployee segTrs from to k
            p' = ss A.! p
        SS.put p'
        liftIO $ print p'

someFunc = do
    [n, q] <- fmap read . words <$> getLine
    ups <- forM [1..n-1] $ const $ do
        [u,p] <- fmap read . words <$> getLine
        return (p,u)
    salaries <- fmap (read::String->Int) . words <$> getLine
    let empTree = buildEmpTree (G.buildG (1,n) ups) 1
        pre = A.listArray (1,n) $ preorderTraversal empTree
        preIdx = A.array (1,n) . fmap (\(i, (j, k)) -> (j, i)) $ A.assocs pre
        ss = A.listArray (1,n) . fmap fst . L.sortBy (comparing snd) $ zip [1..] salaries
        es = A.array (1,n) . fmap (\(a,b) -> (b,a)) $ A.assocs ss
        segTrs = A.listArray (0,n) . scanl (\x y -> updateSegTree (es A.! y) x) (emptySegTree 1 n) . fmap fst $ A.elems pre
    SS.runStateT (query q segTrs pre preIdx ss) 0

        
        



