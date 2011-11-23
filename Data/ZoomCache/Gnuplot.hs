{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
-- Module      : Data.ZoomCache.Gnuplot
-- Copyright   : Alex Lang
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Alex Lang <me@alang.ca>
-- Stability   : unstable
-- Portability : unknown
--
-- Plotting zoom-cache files with gnuplot
----------------------------------------------------------------------

module Data.ZoomCache.Gnuplot
    ( getStreams
    , candlePlotData
    , candlePlot
    , avgPlot
    , mavgPlot
    , bollingerPlot
    , linePlot
    ) where

import Control.Arrow ((***))
import Control.Monad
import Data.Functor ((<$>))
import Data.Maybe
import Data.ByteString (ByteString)
import Data.Monoid
import Debug.Trace

import qualified Data.Iteratee as I
import Data.ZoomCache as Z
import Data.ZoomCache.Numeric.Types as Z
import Graphics.Gnuplot.Advanced
import qualified Graphics.Gnuplot.Graph.TwoDimensional as Graph
import qualified Graphics.Gnuplot.Plot.TwoDimensional as Plot
import qualified Graphics.Gnuplot.Value.Atom as Atom
import qualified Graphics.Gnuplot.Value.Tuple as Tuple

import Unsafe.Coerce

----------------------------------------------------------------------

singleton :: a -> [a]
singleton = (:[])

instance Tuple.C TimeStamp where
    text = singleton . shows . Z.unTS
instance Atom.C TimeStamp where

candlePlotData :: [Summary Double]
               -> [(Z.TimeStamp, (Double, Double, Double, Double))]
candlePlotData zsums =
    map getSummaryCandleVals zsums

candlePlot :: (Tuple.C a, Atom.C a) => [(Z.TimeStamp, (a, a, a, a))]
           -> Plot.T Z.TimeStamp a
candlePlot data_ = Plot.list Graph.candleSticks data_

avgPlot :: [Summary Double] -> Plot.T Z.TimeStamp Double
avgPlot zsums = Plot.list Graph.lines avgs
  where
    avgs = map getSummaryAvg zsums

----------------------------------------------------------------------

linePlot :: [(Z.TimeStamp, Double)] -> Plot.T Z.TimeStamp Double
linePlot = Plot.list Graph.lines

----------------------------------------------------------------------

instance Num Z.TimeStamp where
    a + b = Z.TS $ Z.unTS a + Z.unTS b
    a - b = Z.TS $ Z.unTS a - Z.unTS b
    a * b = Z.TS $ Z.unTS a * Z.unTS b
    negate a = Z.TS . negate $ Z.unTS a
    abs a = Z.TS . abs $ Z.unTS a
    signum a = Z.TS . signum $ Z.unTS a
    fromInteger i = Z.TS $ fromInteger i

instance Real Z.TimeStamp where
    toRational a = toRational $ Z.unTS a

instance Enum Z.TimeStamp where
    toEnum i = Z.TS $ toEnum i
    fromEnum a = fromEnum $ Z.unTS a

instance Integral Z.TimeStamp where
    quotRem a b = Z.TS *** Z.TS $ Z.unTS a `quotRem` Z.unTS b
    toInteger a = toInteger $ Z.unTS a

----------------------------------------------------------------------

data AvgQueue = AvgQueue Z.TimeStamp [(Z.TimeStamp, Double)] [(Z.TimeStamp, Double)] Int

push :: (Z.TimeStamp, Double) -> AvgQueue
     -> AvgQueue
push a (AvgQueue m [] [] _)
    = AvgQueue m [a] [] 1
push a@(t,d) (AvgQueue m xs'@((oldT, _):xs) ys l)
    | t - m > oldT = push (t,d) $ AvgQueue m xs ys (l-1)
    | otherwise = AvgQueue m xs' (a:ys) (l+1)
push a (AvgQueue m [] ys l)
    = push a (AvgQueue m (reverse ys) [] l)

queueAvg :: AvgQueue -> Double
queueAvg (AvgQueue m xs ys l) = realToFrac (sum (map snd xs)
                                            + sum (map snd ys))
                                / realToFrac l

avgEmptyQueue :: Z.TimeStamp -> AvgQueue
avgEmptyQueue m = AvgQueue m [] [] 0

totalTime :: [Summary Double] -> Maybe Z.TimeStamp
totalTime [] = Nothing
totalTime l = Just $ globalClose - globalOpen
  where
    globalClose = Z.summaryExitTime $ last l
    globalOpen = Z.summaryEntryTime $ head l

mavgPlot :: forall a. [Summary Double] -> Plot.T Z.TimeStamp Double
mavgPlot zsums = Plot.list Graph.lines . snd $
                       foldl mavg (avgEmptyQueue window, []) zsums
  where
    window = (fromMaybe (error "Trying to draw an empty plot") $
                        totalTime zsums) `div` Z.TS 10
    mavg :: (AvgQueue, [(Z.TimeStamp, Double)]) -> Summary Double
         -> (AvgQueue, [(Z.TimeStamp, Double)])
    mavg (queue, l) dsum =
        update $ getSummaryAvg dsum
      where
        update (timeStamp, avg) = (newQueue, (timeStamp, queueAvg newQueue):l)
          where
            newQueue = push (timeStamp, avg) queue

----------------------------------------------------------------------

data BoundedQueue a = BoundedQueue Z.TimeStamp
    [((Z.TimeStamp, Z.TimeStamp), a)] [((Z.TimeStamp, Z.TimeStamp), a)]

bqPush :: ((Z.TimeStamp, Z.TimeStamp), a) -> BoundedQueue a -> BoundedQueue a
bqPush a (BoundedQueue m [] [])
    = BoundedQueue m [a] []
bqPush a@((o, c), v) (BoundedQueue m xs'@(((oldO, oldC), oldV):xs) ys)
    | c - m > oldC = bqPush a $ BoundedQueue m xs ys
    | (oldC >= c-m) && (c-m > oldO) = BoundedQueue m
                                      (((c - m, oldC), oldV):xs) (a:ys)
    | otherwise = BoundedQueue m xs' (a:ys)
bqPush a (BoundedQueue m [] ys)
    = bqPush a $ BoundedQueue m (reverse ys) []

queueAvgVar :: BoundedQueue (Double, Double) -> (Double, Double)
queueAvgVar (BoundedQueue _ xs ys) =
    divWeight $ foldl avgFolder (Z.TS 0, 0, 0) (xs ++ ys)
    where
      divWeight (w, accM, accV) = (accM/(realToFrac w), accV/(realToFrac w))
      avgFolder :: (Z.TimeStamp, Double, Double)
                -> ((Z.TimeStamp, Z.TimeStamp), (Double, Double))
                -> (Z.TimeStamp, Double, Double)
      avgFolder (weight, accMean, accVar) ((o, c), (m, s2)) =
          ( (weight + c - o)
          , (accMean + (m * realToFrac (c - o)))
          , (accVar + (s2 * realToFrac (c - o)))
          )

emptyAvgVarQueue :: Z.TimeStamp -> BoundedQueue (Double, Double)
emptyAvgVarQueue m = BoundedQueue m [] []

bollingerPlot :: forall a. [Summary Double] -> Plot.T Z.TimeStamp Double
bollingerPlot dsums = mavg `mappend` upperBB `mappend` lowerBB
  where
    window = (fromMaybe (error "Trying to draw an empty plot") $
                        totalTime dsums) `div` Z.TS 10
    avgsVars = map getSummaryAvgVar dsums
    movingAvgsVars :: [(Z.TimeStamp, (Double, Double))]
    movingAvgsVars = snd $ foldl folder (emptyAvgVarQueue window, []) avgsVars
    mavg = Plot.list Graph.lines $ map (\(t, (m, s)) -> (t, m)) movingAvgsVars
    upperBB = Plot.list Graph.lines $
              map (\(t, (m, s)) -> (t, m + (2 * sqrt s))) movingAvgsVars
    lowerBB = Plot.list Graph.lines $
              map (\(t, (m, s)) -> (t, m - (2 * sqrt s))) movingAvgsVars
    folder :: ( BoundedQueue (Double, Double)
              , [(Z.TimeStamp, (Double, Double))])
           -> (Z.TimeStamp, Z.TimeStamp, Double, Double)
           -> ( BoundedQueue (Double, Double)
              , [(Z.TimeStamp, (Double, Double))])
    folder (queue, l) (o, c, avg, var) =
        (newQueue, (timeStamp, queueAvgVar newQueue):l)
      where
        newQueue = bqPush ((o, c), (avg, var)) queue
        timeStamp = (c + o) `div` 2

----------------------------------------------------------------------

-- plotSummaries :: Atom.C a => Int -> [Z.Stream a] -> [Attribute] -> IO ()
-- plotSummaries lvl streams attrs = plotListStyle attrs
--                         (defaultStyle{plotType = CandleSticks})
--                         candles
--   where
--     candles = map getSummaryCandleVals $
--                 mapMaybe (maybeSummaryLevel lvl) streams

getStreams :: FilePath -> Z.TrackNo -> IO [Z.Stream]
getStreams fp tn =
    flip I.fileDriverRandom fp $
             (I.joinI $ (enumCacheFile standardIdentifiers) I.stream2stream)

-- As things stand, we are doing too much processing after running the
-- iteratee. Most of it can be moved before.

maybeSummaryLevel :: Int -> Z.Stream -> Maybe ZoomSummary
maybeSummaryLevel _ (StreamPacket _ _ _) = Nothing
maybeSummaryLevel lvl (StreamSummary file tn zs@(ZoomSummary s)) =
    case Z.summaryLevel s == lvl of
      True  -> Just zs
      False -> Nothing

getSummaryCandleVals :: Summary Double
                     -> (Z.TimeStamp, (Double, Double, Double, Double))
getSummaryCandleVals dsum =
    ( (openT + closeT) `div` 2
    , ( Z.numEntry sData
      , Z.numMin   sData
      , Z.numMax   sData
      , Z.numExit  sData
      )
    )
  where
    sData = summaryData dsum
    openT = Z.summaryEntryTime dsum
    closeT = Z.summaryExitTime dsum

getSummaryAvg :: Summary Double -> (Z.TimeStamp, Double)
getSummaryAvg dsum =
    ( (openT + closeT) `div` 2
    , Z.numAvg $ Z.summaryData dsum
    )
  where
    openT = summaryEntryTime dsum
    closeT = summaryExitTime dsum

getSummaryAvgVar :: Summary Double
                 -> (Z.TimeStamp, Z.TimeStamp, Double, Double)
getSummaryAvgVar dsum =
    ( openTime
    , closeTime
    , avg
    , var
    )
  where
    sData = summaryData dsum
    avg = Z.numAvg sData
    rms = Z.numRMS sData
    openTime = Z.summaryEntryTime dsum
    closeTime = Z.summaryExitTime dsum
    var = (rms * rms) - (avg * avg)

-- data ZoomNumDict a where
--     ZoomNumDict :: ZoomNum a => ZoomNumDict a
--     NotZoomNumDict :: ZoomNumDict a

-- class IsZoomNum a where
--     isZoomNum :: ZoomNumDict a

-- instance ZoomNum a => IsZoomNum (ZoomNumDict a) where
--     isZoomNum = ZoomNumDict

-- instance ZoomReadable a => IsZoomNum a where
--     isZoomNum = NotZoomNumDict

-- toMDouble :: forall a. (ZoomNumDict a) => a -> Maybe Double
-- toMDouble a = case (isZoomNum :: ZoomNumDict a) of
--                 ZoomNumDict -> Just $ realToFrac a
--                 NotZoomNumDict -> Nothing
