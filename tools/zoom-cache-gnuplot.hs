module Main (
    main
) where

import Data.ZoomCache.Gnuplot (plot)

data ParseError = ParseError

parseTrack :: String -> Either ParseError (FilePath, TrackNo, Int)
parseTrack s =
    case length w of
      3 -> case w of
             [w1, w2, w3] -> Right (w1, read w2, read w3)
             _ -> error "This really should never happen"
      _ ->  Left ParseError
  where
    w = words s
    words :: String -> [String]
    words s = case dropWhile (==':') s of
                "" -> []
                s' -> w : words s''
      where (w, s'') =
        break (==':') s'

main :: IO ()
main = do
    args <- getArgs
    mapM process args
  where
    process :: String -> IO ()
    process s = do
        let (fp, tn, lvl) = either error id parseTrack $ parseTrack s
        plot fp tn lvl