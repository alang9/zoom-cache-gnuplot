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
    ) where

import Data.Maybe

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
    avgs = map getSummaryAvgs $
             mapMaybe (maybeSummaryLevel lvl) streams

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
    case Z.summaryLevel sum of
      lvl -> Just sum
      _   -> Nothing
maybeSummaryLevel _ Z.StreamNull = Nothing

getSummaryCandleVals :: Z.Summary a -> ( Z.TimeStamp
                                       , (a, a, a, a))
getSummaryCandleVals s = ( Z.summaryCloseTime s
                         , ( Z.summaryOpen s
                           , Z.summaryMin s
                           , Z.summaryMax s
                           , Z.summaryClose s
                         ))

getSummaryAvgs :: Z.Summary a -> (Z.TimeStamp, Double)
getSummaryAvgs s = ( Z.summaryCloseTime s
                   , Z.summaryAvg s
                   )