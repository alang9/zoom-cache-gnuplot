module Main (
    main
) where

import Data.Maybe (fromMaybe, catMaybes)
import System.Environment (getArgs)
import System.Console.GetOpt

import Graphics.Gnuplot.Simple
import Graphics.Gnuplot.Advanced

import Graphics.Gnuplot.Value.Tuple (C(..))
import Data.ZoomCache.Read (getTrackType, getCacheFile)
import Data.Iteratee.ZoomCache (Stream)
import Data.ZoomCache.Common (TrackType(..), TrackNo, TimeStamp)

import Data.ZoomCache.Gnuplot

data ParseError = ParseError

parseTrack :: String -> Either ParseError (FilePath, TrackNo, Int)
parseTrack arg =
    case w of
      [w1, w2, w3] -> Right (w1, read w2, read w3)
      _ ->  Left ParseError
  where
    w = words arg
    words :: String -> [String]
    words s = case dropWhile (==':') s of
                "" -> []
                s' -> w : words s''
                    where
                      (w, s'') = break (==':') s'


-- Options record, only gnuplot options for now
data Options = Options
    { gnuplotOpts :: [Attribute]
    , candleSticks :: [(FilePath, TrackNo, Int)]
    , avgs :: [(FilePath, TrackNo, Int)]
    }

defaultOptions = Options
    { gnuplotOpts = []
    , candleSticks = []
    , avgs = []
    }

parseCustom :: String -> Attribute
parseCustom s =
    Custom s1 [tail s2]
      where (s1, s2) = break (==':') s


options :: [OptDescr (Options -> Options)]
options =
    [ Option ['g'] ["gnuplot"]
        (OptArg ((\ f opts -> opts { gnuplotOpts = parseCustom f : gnuplotOpts opts }) . fromMaybe "gnuplot")
                             "KEY:VALUE")
        "gnuplot KEY:VALUE"
    , Option ['c'] ["candlesticks"]
        (OptArg ((\ f opts ->
          opts { candleSticks = either (error "bad command line syntax")
                                 (: candleSticks opts) $ parseTrack f  }) .
                               fromMaybe "candlesticks")
          "FILE:TRACKNO:SUMMARYLVL")
        "candelsticks FILE:TRACKNO:SUMMARYLVL"
    ]

parseOpts :: [String] -> IO (Options, [String])
parseOpts argv =
    case getOpt Permute options argv of
      (o, n, []) -> return (foldl (flip id) defaultOptions o, n)
      (_, _, errs) -> ioError (userError (concat errs ++ usageInfo header options))
        where header = "Usage: zoom-cache-gnuplot ..."



main :: IO ()
main = do
    args <- getArgs
    (opts, remainder) <- parseOpts args
    cPlots <- fmap (mconcat . catMaybes) . mapM candleProcess $ candleSticks opts
    aPlots <- fmap (mconcat . catMaybes) . mapM avgProcess $ avgs opts
    let plots = cPlots `mappend` aPlots
    exitWith =<< Plot.plot X11.cons plots
  where
    candleProcess :: (FilePath, TrackNo, Int) -> IO (Maybe (Plot.T TimeStamp Double))
    candleProcess (fp, tn, lvl) = do
        cf <- getCacheFile fp
        case getTrackType tn cf of
          Just ZInt -> do
              streams <- getStreams fp tn :: IO [Stream Int]
              let cData = candlePlotData streams lvl
                  cData' = map (\(t,(a,b,c,d))
                                -> (t, ( realToFrac a
                                       , realToFrac b
                                       , realToFrac c
                                       , realToFrac d)
                                    :: (Double, Double, Double, Double)))
                             cData
              return . Just $ candlePlot cData'
          Just ZDouble -> do
              streams <- getStreams fp tn :: IO [Stream Double]
              let cData = candlePlotData streams lvl
              return . Just $ candlePlot cData
          Nothing -> return Nothing
    avgProcess :: (FilePath, TrackNo, Int) -> IO (Maybe (Plot.T TimeStamp Double))
    avgProcess (fp, tn, lvl) = do
        cf <- getCacheFile fp
        case getTrackType tn cf of
          Just ZInt -> do
              streams <- getStreams fp tn :: IO [Stream Int]
              return . Just $ avgPlot streams lvl
          Just ZDouble -> do
              streams <- getStreams fp tn :: IO [Stream Double]
              return . Just $ avgPlot streams lvl
          Nothing -> return Nothing
