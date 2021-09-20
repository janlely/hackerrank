{-# LANGUAGE MultiWayIf #-}
module Lib
    ( someFunc
    ) where


import Control.Monad
import qualified Data.List as L
import qualified Data.Array as A
import Data.Maybe
import Debug.Trace


data Heap
    = Empty 
    | Tree {  _rank  :: Int
           ,  _size  :: Int
           ,  _value :: Int
           , _left   :: Heap
           , _right  :: Heap
           , _f      :: Int -> Int -> Bool
           }


showHeap :: Heap -> String
showHeap Empty = ""
showHeap t = show' t 0
    where show' (Tree _ _ v l r _) n = replicate n ' ' ++ "value: " ++  show v ++ "\n" ++ replicate n ' ' ++  "left: " ++ "\n" ++ replicate n ' ' ++ show' l (n+4) ++ "right: " ++ "\n" ++ replicate n ' ' ++ show' r (n+4)
          show' Empty _ = ""

isEmpty :: Heap -> Bool
isEmpty Empty = True
isEmpty _ = False

singleton :: Int -> (Int -> Int -> Bool) -> Heap
singleton v f = Tree { _rank = 1
                     , _size = 1
                     , _value = v
                     , _left = Empty
                     , _right = Empty
                     , _f = f}

union :: Heap -> Heap -> Heap
union h Empty = h
union Empty h = h
union h1 h2 = if f (_value h1) (_value h2)
                 then makeHeap (_value h1) f (_left h1) (union (_right h1) h2)
                 else makeHeap (_value h2) f (_left h2) (union (_right h2) h1)
  where f = _f h1

insertHeap :: Int -> (Int -> Int -> Bool) -> Heap -> Heap
insertHeap v f h = union h $ singleton v f

viewHead :: Heap -> Maybe Int
viewHead Empty = Nothing
viewHead h = Just $ _value h

popHeap :: Heap -> Heap
popHeap Empty = Empty
popHeap (Tree _ _ _ Empty h _) = h
popHeap (Tree _ _ _ h Empty _) = h
popHeap (Tree _ _ _ h1 h2 _) = union h1 h2


makeHeap :: Int -> (Int -> Int -> Bool) -> Heap -> Heap -> Heap
makeHeap v f h1 h2 =
    let ra = rank h1
        rb = rank h2
        s = size h1 + size h2 + 1
     in if ra > rb
           then Tree (rb + 1) s v h1 h2 f
           else Tree (ra + 1) s v h2 h1 f

size :: Heap -> Int
size Empty = 0
size h = _size h

rank :: Heap -> Int
rank Empty = 0
rank h = _rank h

buildHeaps:: [(Int, Int)] -> A.Array Int (Heap, Heap)
buildHeaps qs = res 
    where trs = L.scanl f (Empty, Empty) qs
          res = A.listArray (0, length qs) trs
          f (maxh, minh) (i,v)
            | v > 0 = if | size minh' > size maxh'     -> let item = fromJust $ viewHead minh' in (insertHeap item (>) maxh', popHeap minh') 
                         | size maxh' - size minh' > 1 -> let item = fromJust $ viewHead maxh' in (popHeap maxh', insertHeap item (<) minh')
                         | otherwise                           -> (maxh', minh')
            | otherwise = res A.! (i+v)
                where (maxh', minh') = if | isEmpty maxh ->  (insertHeap v (>) maxh, minh)
                                          | isEmpty minh ->  if v > (fromJust $ viewHead maxh) then (maxh, insertHeap v (<) minh) else (singleton v (>), singleton (fromJust $ viewHead maxh) (<))
                                          | v < fromJust (viewHead minh) -> (insertHeap v (>) maxh, minh)
                                          | otherwise                    -> (maxh, insertHeap v (<) minh)

someFunc = do
    t <- read <$> getLine
    qs <- replicateM t $ read <$> getLine
    let sqs = zip [1..] $ (L.nub . L.sort . L.filter (>0)) qs
        heaps = buildHeaps $ zip [1..] qs
    -- forM ((\(a,b) -> "heap:\n" ++ showHeap a ++ "\n" ++ showHeap b ++ "\n") <$> take 11 (A.elems heaps)) putStrLn
    forM [1..t] (\n -> print $ (fromJust . viewHead . fst . (heaps A.!)) n)
