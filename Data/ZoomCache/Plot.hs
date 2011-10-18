import Data.ZoomCache

plot :: FilePath -> TrackNo -> Int -> IO ()
plot = undefined


zoomEither :: (Stream a -> b) -> FilePath -> TrackNo -> IO b
zoomEither fun fp tn = do
  cf <- getCacheFile fp
  let t = getTrackType tn cf
  case t of
    Just ZDouble -> I.fileDriverRandom (mapTrack tn (fun :: Stream Double -> IO ())) fp
    Just ZInt -> I.fileDriverRandom (mapTrack tn (fun :: Stream Int -> IO ())) fp
    Nothing -> fail "Invalid Track"


mapTrack :: (Functor m, MonadIO m, ZReadable a)
          => TrackNo -> (Stream a -> )
          -> Iteratee [Word8] m ()
mapTrack n = I.joinI . (enumStreamFromCF n) . I.mapChunks

-- I think adopting this order of arguments makes more sense
zoomListPlotLevel :: C a => FilePath -> TrackNo -> Int -> IO [Stream a]
zoomListPlotLevel fp tn lvl =
    I.fileDriverRandom (I.joinI $ enumStreamFromCF tn getChunks) fp
