module Lib
    ( solve 
    ) where

import Control.Monad
import Data.List (sortBy)

dotProduct :: (Double, Double) -> (Double, Double) -> (Double, Double) -> Double
dotProduct (x0, y0) (x1, y1) (x2, y2) = let (x1', y1') = (x1-x0, y1-y0)
                                            (x2', y2') = (x2-x0, y2-y0)
                                         in x1'*x2' + y1'*y2'
    

crossProduct :: (Double, Double) -> (Double, Double) -> (Double, Double) -> Double
crossProduct (x0, y0) (x1, y1) (x2, y2) = let (x1', y1') = (x1-x0, y1-y0)
                                              (x2', y2') = (x2-x0, y2-y0)
                                           in x1'*y2' - y1'*x2'

getAngle :: (Double, Double) -> (Double, Double) -> (Double, Double) -> Double
getAngle z a b = let d = dotProduct z a b
                     c = crossProduct z a b
                     angle = atan2 c d
                  in if angle < 0
                        then angle + 2 * pi 
                        else angle


sortByAngle :: [(Double, Double)] -> (Double, Double) -> [(Double, Double)]
sortByAngle [] _ = []
sortByAngle [a] _ = [a]
sortByAngle (a:as) z = a: sortBy f as
    where f xy1 xy2 = let angle1 = getAngle z a xy1
                          angle2 = getAngle z a xy2
                       in compare angle1 angle2 

solve = do
    n <- read <$> getLine
    points <- forM [1..n] $ const $ do
        [x,y] <- fmap (fmap read) (words <$> getLine)
        return (x,y)
    let minx = minimum $ fst <$> points
        maxx = maximum $ fst <$> points
        miny = minimum $ snd <$> points
        maxy = maximum $ snd <$> points 
        (cx, cy) = ((minx + maxx) / 2, (miny + maxy) / 2)
        points' = cycle $ sortByAngle points (cx, cy)
        edgePairs = take (length points) $ zip3 points' (drop 1 points') (drop 2 points')
        cps = map (\(a,b,c) -> crossProduct b a c) edgePairs
    if all (>= 0) cps || all (<= 0) cps
       then putStrLn "NO"
       else putStrLn "YES"
    

