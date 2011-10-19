{-# LANGUAGE ExistentialQuantification #-}
module Main (
    main
) where

import Data.Maybe (fromMaybe, catMaybes)
import System.Environment (getArgs)
import System.Console.GetOpt

import Graphics.Gnuplot.Simple
import Graphics.Gnuplot.Value.Tuple (C(..))
import Data.ZoomCache.Read (getTrackType, getCacheFile)
import Data.Iteratee.ZoomCache (Stream)
import Data.ZoomCache.Common (TrackType(..), TrackNo)

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

data SomeC = forall a. C a => SomeC a

instance C SomeC where
    text (SomeC a) = text a

main :: IO ()
main = do
    args <- getArgs
    (opts, remainder) <- parseOpts args
    -- mapM_ (process opts) remainder
    cPlots <- fmap catMaybes . mapM candleProcess $ candleSticks opts
    let plots = cPlots
    case plots of
      [] -> error "Cannot produce empty plot"
      _ -> plotListsStyle (gnuplotOpts opts) plots
  where
    candleProcess :: (FilePath, TrackNo, Int) -> IO (Maybe (PlotStyle, [SomeC]))
    candleProcess (fp, tn, lvl) = do
        cf <- getCacheFile fp
        case getTrackType tn cf of
          Just ZInt -> do
              streams <- getStreams fp tn :: IO [Stream Int]
              let (s, l) = candlePlots streams lvl
              return $ Just (s, map SomeC l)
          Just ZDouble -> do
              streams <- getStreams fp tn :: IO [Stream Double]
              let (s, l) = candlePlots streams lvl
              return $ Just (s, map SomeC l)
          Nothing -> return Nothing
