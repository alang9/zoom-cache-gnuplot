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
    ( plotSummaries
    , getStreams
    ) where

import Data.Maybe
import qualified Data.Iteratee as I

import qualified Data.Iteratee.ZoomCache as Z
import qualified Data.ZoomCache.Common as Z
import qualified Data.ZoomCache.Read as Z
import qualified Data.ZoomCache.Summary as Z
import Graphics.Gnuplot.Value.Tuple
import Graphics.Gnuplot.Simple

----------------------------------------------------------------------

singleton :: a -> [a]
singleton = (:[])

instance C Z.TimeStamp where text = singleton . shows . Z.unTS

plotSummaries :: C a => Int -> [Z.Stream a] -> [Attribute] -> IO ()
plotSummaries lvl streams attrs = plotListStyle attrs
                        (defaultStyle{plotType = CandleSticks})
                        candles
  where
    candles = map getSummaryCandleVals $
                mapMaybe (maybeSummaryLevel lvl) streams

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

getSummaryCandleVals :: Z.Summary a -> (Z.TimeStamp, (a, a, a, a))
getSummaryCandleVals s = ( Z.summaryCloseTime s
                         , ( Z.summaryOpen s
                           , Z.summaryMin s
                           , Z.summaryMax s
                           , Z.summaryClose s
                         ))
