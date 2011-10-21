{-# LANGUAGE ScopedTypeVariables #-}
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
    ) where

import Control.Arrow ((***))
import Data.Maybe
import Data.Monoid

import qualified Data.Iteratee as I
import qualified Data.Iteratee.ZoomCache as Z
import qualified Data.ZoomCache.Common as Z
import qualified Data.ZoomCache.Read as Z
import qualified Data.ZoomCache.Summary as Z
import Graphics.Gnuplot.Advanced
import Graphics.Gnuplot.Simple
import qualified Graphics.Gnuplot.Graph.TwoDimensional as Graph
import qualified Graphics.Gnuplot.Plot.TwoDimensional as Plot
import qualified Graphics.Gnuplot.Value.Atom as Atom
import qualified Graphics.Gnuplot.Value.Tuple as Tuple

----------------------------------------------------------------------

singleton :: a -> [a]
singleton = (:[])

instance Tuple.C Z.TimeStamp where
    text = singleton . shows . Z.unTS
instance Atom.C Z.TimeStamp where

candlePlotData :: [Z.Stream a] -> Int
               -> [(Z.TimeStamp, (a, a, a, a))]
candlePlotData streams lvl =
    map getSummaryCandleVals $
                mapMaybe (maybeSummaryLevel lvl) streams

candlePlot :: (Tuple.C a, Atom.C a) => [(Z.TimeStamp, (a, a, a, a))]
           -> Plot.T Z.TimeStamp a
candlePlot data_ =
    Plot.list Graph.candleSticks data_

avgPlot :: Atom.C a => [Z.Stream a] -> Int
        -> Plot.T Z.TimeStamp Double
avgPlot streams lvl =
    Plot.list Graph.lines avgs
  where
    avgs = map getSummaryAvg $
             mapMaybe (maybeSummaryLevel lvl) streams

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

totalTime :: [Z.Summary a] -> Maybe Z.TimeStamp
totalTime [] = Nothing
totalTime l = Just $ globalClose - globalOpen
  where
    globalClose = Z.summaryCloseTime (last l)
    globalOpen = Z.summaryOpenTime (head l)

mavgPlot :: forall a. [Z.Stream a]
         -> Int -> Plot.T Z.TimeStamp Double
mavgPlot streams lvl = Plot.list Graph.lines . snd $
                       foldl mavg (avgEmptyQueue window, []) summaries
  where
    summaries :: [Z.Summary a]
    summaries = mapMaybe (maybeSummaryLevel lvl) streams
    window = (fromMaybe (error "Trying to draw an empty plot") $
                        totalTime summaries) `div` Z.TS 10
    mavg :: (AvgQueue, [(Z.TimeStamp, Double)]) -> Z.Summary a
         -> (AvgQueue, [(Z.TimeStamp, Double)])
    mavg (queue, l) s = (newQueue, (timeStamp, queueAvg newQueue):l)
      where
        (timeStamp, avg) = getSummaryAvg s
        newQueue = push (timeStamp, avg) queue

----------------------------------------------------------------------

data BoundedQueue a = BoundedQueue Z.TimeStamp [(Z.TimeStamp, a)] [(Z.TimeStamp, a)] Int

bqPush :: (Z.TimeStamp, a) -> BoundedQueue a
     -> BoundedQueue a
bqPush a (BoundedQueue m [] [] _)
    = BoundedQueue m [a] [] 1
bqPush a@(t, v) (BoundedQueue m xs'@((oldT, _):xs) ys l)
    | t - m > oldT = bqPush (t, v) $ BoundedQueue m xs ys (l-1)
    | otherwise = BoundedQueue m xs' (a:ys) (l+1)
bqPush a (BoundedQueue m [] ys l)
    = bqPush a (BoundedQueue m (reverse ys) [] l)

queueAvgVar :: BoundedQueue (Double, Double) -> (Double, Double)
queueAvgVar (BoundedQueue m xs ys l) =
    (takeAverage *** takeAverage) . unzip $ map snd (xs ++ ys)
    where
      takeAverage :: [Double] -> Double
      takeAverage = (/ realToFrac l) . sum

emptyAvgVarQueue :: Z.TimeStamp -> BoundedQueue (Double, Double)
emptyAvgVarQueue m = BoundedQueue m [] [] 0

bollingerPlot :: forall a. [Z.Stream a] -> Int -> Plot.T Z.TimeStamp Double
bollingerPlot streams lvl = mavg `mappend` upperBB `mappend` lowerBB
  where
    summaries :: [Z.Summary a]
    summaries = mapMaybe (maybeSummaryLevel lvl) streams
    window = (fromMaybe (error "Trying to draw an empty plot") $
                        totalTime summaries) `div` Z.TS 10
    avgsVars = map getSummaryAvgVar summaries
    movingAvgsVars :: [(Z.TimeStamp, (Double, Double))]
    movingAvgsVars = snd $ foldl folder (emptyAvgVarQueue window, []) avgsVars
    mavg = Plot.list Graph.lines $ map (\(t, (m, s)) -> (t, m)) movingAvgsVars
    upperBB = Plot.list Graph.lines $ map (\(t, (m, s)) -> (t, m+2*s)) movingAvgsVars
    lowerBB = Plot.list Graph.lines $ map (\(t, (m, s)) -> (t, m-2*s)) movingAvgsVars
    folder :: (BoundedQueue (Double, Double), [(Z.TimeStamp, (Double, Double))])
           -> (Z.TimeStamp, Double, Double)
           -> (BoundedQueue (Double, Double), [(Z.TimeStamp, (Double, Double))])
    folder (queue, l) (timeStamp, avg, var) =
        (newQueue, (timeStamp, queueAvgVar newQueue):l)
      where
        newQueue = bqPush (timeStamp, (avg, var)) queue

----------------------------------------------------------------------

-- plotSummaries :: Atom.C a => Int -> [Z.Stream a] -> [Attribute] -> IO ()
-- plotSummaries lvl streams attrs = plotListStyle attrs
--                         (defaultStyle{plotType = CandleSticks})
--                         candles
--   where
--     candles = map getSummaryCandleVals $
--                 mapMaybe (maybeSummaryLevel lvl) streams

getStreams :: Z.ZReadable a => FilePath -> Z.TrackNo -> IO [Z.Stream a]
getStreams fp tn =
    I.fileDriverRandom (I.joinI $ Z.enumStreamFromCF tn I.getChunks) fp

-- As things stand, we are doing too much processing after running the
-- iteratee. Most of it can be moved before.

maybeSummaryLevel :: Int -> Z.Stream a -> Maybe (Z.Summary a)
maybeSummaryLevel _ (Z.StreamPacket _ _ _) = Nothing
maybeSummaryLevel lvl (Z.StreamSummary file tn sum) =
    case Z.summaryLevel sum == lvl of
      True  -> Just sum
      False -> Nothing
maybeSummaryLevel _ Z.StreamNull = Nothing

getSummaryCandleVals :: Z.Summary a -> ( Z.TimeStamp
                                       , (a, a, a, a))
getSummaryCandleVals s = ( (Z.summaryCloseTime s + Z.summaryOpenTime s)
                             `div` 2
                         , ( Z.summaryOpen s
                           , Z.summaryMin s
                           , Z.summaryMax s
                           , Z.summaryClose s
                         ))

getSummaryAvg :: Z.Summary a -> (Z.TimeStamp, Double)
getSummaryAvg s = ( ((Z.summaryCloseTime s) + (Z.summaryOpenTime s)) `div` 2
                   , Z.summaryAvg s
                   )

getSummaryAvgVar :: Z.Summary a -> (Z.TimeStamp, Double, Double)
getSummaryAvgVar s = ( (openTime + closeTime) `div` 2
                     , avg
                     , (rms * rms) - (avg * avg)
                     )
  where
    avg = Z.summaryAvg s
    rms = Z.summaryRMS s
    openTime = Z.summaryOpenTime s
    closeTime = Z.summaryCloseTime s