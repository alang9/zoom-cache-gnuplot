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

import qualified Data.Iteratee as I

import Data.ZoomCache.Common
import Graphics.Gnuplot.Simple

plot :: FilePath -> TrackNo -> Int -> IO ()
plot fp tn lvl = do
  streams <- getStreams fp tn
  let candles = map getSummaryCandleVals $ mayMaybe maybeSummaryLevel streams
  plotListStyle [] (defaultStyle{plotType = CandleSticks}) candles

zoomEither :: (Stream a -> b) -> FilePath -> TrackNo -> IO b
zoomEither fun fp tn = do
  cf <- getCacheFile fp
  let t = getTrackType tn cf
  case t of
    Just ZDouble -> I.fileDriverRandom
                       (mapTrack tn (fun :: Stream Double -> IO ()))
                       fp
    Just ZInt -> I.fileDriverRandom
                       (mapTrack tn (fun :: Stream Int -> IO ()))
                       fp
    Nothing -> fail "Invalid Track"


mapTrack :: (Functor m, MonadIO m, ZReadable a)
         => TrackNo -> (Stream a -> )
         -> Iteratee [Word8] m ()
mapTrack n = I.joinI . (enumStreamFromCF n) . I.mapChunks



listStreams :: C a => FilePath -> TrackNo -> IO [Stream a]
listStreams fp tn =
    I.fileDriverRandom (I.joinI $ enumStreamFromCF tn getChunks) fp

-- As things stand, we are doing too much processing after running the
-- iteratee. Most of it can be moved before.

maybeSummaryLevel :: Int -> Stream a -> Maybe (Summary a)
maybeSummaryLevel _ (StreamPacket _ _ _) = Nothing
maybeSummaryLevel lvl (StreamSummary file tn sum) =
    case summaryLevel sum of
      lvl -> Just sum
      _   -> Nothing
maybeSummaryLevel _ StreamNull = Nothing

getSummaryCandleVals :: Summary a -> (TimeStamp, (a, a, a, a))
getSummaryCandleVals s = ( summaryCloseTime s
                         , ( summaryOpen s
                           , summaryMin s
                           , summaryMax s
                           , summaryClose s
                         ))
