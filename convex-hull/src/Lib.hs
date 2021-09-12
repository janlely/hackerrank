module Lib
    ( someFunc
    ) where

import Control.Monad
import Data.List (sortBy, foldl')
import Data.Ord (comparing)
import Debug.Trace

dotProduct :: (Double, Double) -> (Double, Double) -> Double
dotProduct (x1, y1) (x2, y2) = x1*x2 + y1*y2
    

crossProduct :: (Double, Double) -> (Double, Double) -> Double
crossProduct (x1, y1) (x2, y2) = x1*y2 - y1*x2

getAngle :: (Double, Double) -> (Double, Double) -> Double
getAngle a b = let d = dotProduct a b
                   c = crossProduct a b
                   angle = atan2 c d
                in if angle < 0
                      then angle + 2 * pi 
                      else angle


sortByAngle :: [(Double, Double)] -> [(Double, Double)]
sortByAngle []  = []
sortByAngle [a] = [a]
sortByAngle as  = sortBy f as
    where f xy1 xy2 = let angle1 = getAngle (1,0) xy1
                          angle2 = getAngle (1,0) xy2
                       in if angle1 == angle2 
                             then compare (l xy1)  (l xy2)
                             else compare angle1 angle2
          l (x, y) = x*x + y*y


calcPerimeter :: [(Double, Double)] -> Double
calcPerimeter ps = snd $ foldl f (last ps, 0) ps
  where f ((x0,y0), s) (x1,y1) = ((x1,y1), s + (sqrt $ (x1-x0)^2 + (y1-y0)^2))

calcArea :: [(Double, Double)] -> Double
calcArea ps = let (psx, psy) = unzip ps
                  a = ((last psx * head psy) +) . sum $ zipWith (*) (init psx) (tail psy)
                  b = ((head psx * last psy) +) . sum $ zipWith (*) (tail psx) (init psy)
               in 0.5 * (a-b)

isLeft :: (Double, Double) -> (Double, Double) -> (Double, Double) -> Bool
isLeft a b c = calcArea [a,b,c] >= 0

convexHull :: [(Double, Double)] -> [(Double, Double)]
convexHull ps = foldl' f s t
    where f (p2:p1:pts) p3 = if isLeft p1 p2 p3
                                then p3:p2:p1:pts
                                else f (p1:pts) p3
          ys = sortBy (comparing snd) ps
          a0:a1:as = sortByAngle $ (\(x,y) -> let (xy, yy) = head ys in (x - xy, y - yy)) <$> tail ys
          s = [a1, a0, (0,0)]
          t = as

someFunc = do
    n <- read <$> getLine
    points <- forM [1..n] $ const $ do
        [x,y] <- fmap (fmap read) (words <$> getLine)
        return (x,y)
    print $ calcPerimeter $ convexHull points
    


