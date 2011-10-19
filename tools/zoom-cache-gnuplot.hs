module Main (
    main
) where

import System.Environment (getArgs)

import Data.ZoomCache.Gnuplot
import Data.ZoomCache.Read (getTrackType, getCacheFile)
import Data.Iteratee.ZoomCache (Stream)
import Data.ZoomCache.Common (TrackType(..), TrackNo)

data ParseError = ParseError

parseTrack :: String -> Either ParseError (FilePath, TrackNo, Int)
parseTrack arg =
    case length w of
      3 -> case w of
             [w1, w2, w3] -> Right (w1, read w2, read w3)
             _ -> error "This really should never happen"
      _ ->  Left ParseError
  where
    w = words arg
    words :: String -> [String]
    words s = case dropWhile (==':') s of
                "" -> []
                s' -> w : words s''
                    where
                      (w, s'') = break (==':') s'

main :: IO ()
main = do
    args <- getArgs
    mapM_ process args
  where
    process :: String -> IO ()
    process s = do
        let (fp, tn, lvl) = either (error "badly formed argument") id $ parseTrack s
        cf <- getCacheFile fp
        case getTrackType tn cf of
          Just ZInt -> do
              streams <- getStreams fp tn :: IO [Stream Int]
              plotSummaries lvl streams
          Just ZDouble -> do
              streams <- getStreams fp tn :: IO [Stream Int]
              plotSummaries lvl streams
          Nothing -> return ()
