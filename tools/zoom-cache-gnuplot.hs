module Main (
    main
) where

import Data.Maybe (fromMaybe)
import System.Environment (getArgs)
import System.Console.GetOpt

import Graphics.Gnuplot.Simple
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
    }

defaultOptions = Options
    { gnuplotOpts = []
    , candleSticks = []
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
    mapM_ (process opts) remainder
  where
    process :: Options -> String -> IO ()
    process opts s = do
        let (fp, tn, lvl) = either (error "badly formed argument") id $ parseTrack s
        cf <- getCacheFile fp
        case getTrackType tn cf of
          Just ZInt -> do
              streams <- getStreams fp tn :: IO [Stream Int]
              plotSummaries lvl streams $ gnuplotOpts opts
          Just ZDouble -> do
              streams <- getStreams fp tn :: IO [Stream Double]
              plotSummaries lvl streams $ gnuplotOpts opts
          Nothing -> return ()
