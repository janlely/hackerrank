module Lib
    ( treeLive,
    runTreeLiveT
    ) where

import Data.Int
import Data.Bits
import Data.Bifunctor (second, first)
import Debug.Trace (trace)
import Data.List (foldl')
import Data.IORef
import Control.Monad
import qualified Control.Monad.State.Strict as SS
import Control.Monad.IO.Class (liftIO)



data Tree a = Nil | Tree (Tree a, a, Tree a) deriving Show

ruleN :: Int16 -> (Char, Char, Char, Char) -> Char
ruleN n cs = case cs of
              ('X','X','X','X') -> if testBit n 15 then 'X' else '.'
              ('X','X','X','.') -> if testBit n 14 then 'X' else '.'
              ('X','X','.','X') -> if testBit n 13 then 'X' else '.'
              ('X','X','.','.') -> if testBit n 12 then 'X' else '.'
              ('X','.','X','X') -> if testBit n 11 then 'X' else '.'
              ('X','.','X','.') -> if testBit n 10 then 'X' else '.'
              ('X','.','.','X') -> if testBit n 9  then 'X' else '.'
              ('X','.','.','.') -> if testBit n 8  then 'X' else '.'
              ('.','X','X','X') -> if testBit n 7  then 'X' else '.'
              ('.','X','X','.') -> if testBit n 6  then 'X' else '.'
              ('.','X','.','X') -> if testBit n 5  then 'X' else '.'
              ('.','X','.','.') -> if testBit n 4  then 'X' else '.'
              ('.','.','X','X') -> if testBit n 3  then 'X' else '.'
              ('.','.','X','.') -> if testBit n 2  then 'X' else '.'
              ('.','.','.','X') -> if testBit n 1  then 'X' else '.'
              ('.','.','.','.') -> if testBit n 0  then 'X' else '.'

getNodeValue :: Tree Char -> Char
getNodeValue (Tree (_, c, _)) = c

readTree :: String -> Tree Char
readTree [] = Nil
readTree [c] = Tree (Nil, c, Nil)
readTree cs = fst $ head $ foldl' buildFunc [] cs
    where buildFunc trs ch 
            | ch == '(' = (Nil, False):trs
            | ch == ')' = if snd (head trs)
                             then buildNode' (head $ tail trs) (fst $ head trs) : tail (tail trs)
                             else second not (head trs):tail trs
            | otherwise = if snd (head trs)
                             then first (setv ch) (buildNode' (head $ tail trs) (fst $ head trs)) : tail (tail trs)
                             else buildNode (head trs) ch:tail trs
          buildNode :: (Tree Char, Bool) -> Char -> (Tree Char, Bool)
          buildNode (Nil, end) ch = (Tree (leaf ch, '0', Nil), end)
          buildNode (Tree (l, '0', Nil), end) ch = (Tree (l, ch, Nil), end)
          buildNode (Tree (l, c, Nil), end) ch = (Tree (l, c, leaf ch), end)
          buildNode' :: (Tree Char, Bool) -> Tree Char -> (Tree Char, Bool)
          buildNode' (Nil, end) tr = (Tree (tr, '0', Nil), end)
          buildNode' (Tree (l, c, Nil), end) tr = (Tree (l, c, tr), True)
          setv :: Char -> Tree Char -> Tree Char
          setv ch (Tree (l, _, r)) = Tree (l, ch, r)
          leaf :: Char -> Tree Char
          leaf ch = Tree (Nil, ch, Nil)

updateForward :: ((Char, Char, Char, Char) -> Char) -> Int -> Tree Char -> [Tree Char]
updateForward _ 0 tr = []
updateForward rule n tr = let tr' = doUpdate '.' tr
                           in tr' : updateForward rule (n-1) tr'
                          where doUpdate parent (Tree (Nil, c, Nil)) = Tree (Nil, rule (parent, '.', c, '.'), Nil)
                                doUpdate parent (Tree (l, c, r)) = let lv = getNodeValue l
                                                                       rv = getNodeValue r
                                                                    in Tree (doUpdate c l, rule (parent, lv, c, rv), doUpdate c r)

walkThrough :: Tree Char -> String -> Char
walkThrough tr "" = getNodeValue tr
walkThrough (Tree (l, _, r)) ('<':ds) = walkThrough l ds
walkThrough (Tree (l, _, r)) ('>':ds) = walkThrough r ds


        
printTree :: Tree Char -> String
printTree (Tree (Nil, c, Nil)) = [c]
printTree (Tree (l, c, r)) = "(" ++ printTree l ++ [' ', c, ' '] ++ printTree r ++ ")"

getByIndex :: Int -> [a] -> a
getByIndex 0 as = last as
getByIndex n as = as !! (length as + n - 1)

treeLive = do
    rule <- ruleN . read <$> getLine
    initTree <- readTree' <$> getLine
    treeListRef <- newIORef ([initTree],0)
    queries <- read <$> getLine
    forM [1..queries] $ const $ do
        [iterN, pathes] <- words <$> getLine
        (trs, lastIdx) <- readIORef treeListRef
        let n = read iterN 
        if n + lastIdx > 0
           then writeIORef treeListRef (trs ++ updateForward rule (n+lastIdx) (last trs), 0)
           else writeIORef treeListRef (trs, n + lastIdx)
        (trs', idx) <- readIORef treeListRef
        let result = if n+lastIdx > 0
                        then walkThrough (last trs') $ init $ tail pathes
                        else walkThrough (getByIndex (n+lastIdx) trs') $ init $ tail pathes
        putStrLn [result]
  where readTree' = readTree . filter (/= ' ')

treeLiveT :: SS.StateT ([Tree Char], Int) IO ()
treeLiveT = head <$> do
    rule <- liftIO $ (ruleN . read) <$> getLine
    initTree <- liftIO $ readTree' <$> getLine
    SS.put ([initTree], 0)
    queries <- liftIO $ read <$> getLine
    forM [1..queries] $ const $ do
        [iterN, pathes] <- liftIO $ words <$> getLine
        (trs, lastIdx) <- SS.get 
        let n = read iterN 
        if n + lastIdx > 0
           then SS.put (trs ++ updateForward rule (n+lastIdx) (last trs), 0)
           else SS.put (trs, n + lastIdx) 
        (trs', idx) <- SS.get 
        let result = if n+lastIdx > 0
                        then walkThrough (last trs') $ init $ tail pathes
                        else walkThrough (getByIndex (n+lastIdx) trs') $ init $ tail pathes
        liftIO $ putStrLn [result]
  where readTree' = readTree . filter (/= ' ')

runTreeLiveT = SS.runStateT treeLiveT ([], 0)
    


