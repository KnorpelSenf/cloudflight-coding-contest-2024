module LevelOne (main) where

import Control.Applicative (Alternative (empty))
import Control.Monad (when)
import Data.List (maximumBy, nub)
import Data.Maybe (fromJust, isJust, isNothing)
import Debug.Trace (trace)
import Distribution.Simple.Command (OptDescr (BoolOpt))
import MParser
import System.Environment (getArgs)

lvl = "1"

inpath :: Int -> String
inpath i = "level" ++ lvl ++ "/level" ++ lvl ++ "_" ++ show i ++ ".in"

examplepath = "level" ++ lvl ++ "/level" ++ lvl ++ "_example.in"

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> sequence_ [task (inpath i) | i <- [1 .. 5]]
    ["-e"] -> task examplepath
    [i] -> case i of
      ('-' : num) -> task $ inpath (read num :: Int)
      _ -> putStrLn $ "error: unknown option: " ++ show args

cleanNewlines :: String -> String
cleanNewlines = filter (/= '\r')

task :: String -> IO ()
task filepath = do
  input <- cleanNewlines <$> readFile filepath
  let n = read . head . lines $ input :: Int
  let problems = parseProblems (unlines . tail . lines $ input)
  let results = map processProblem problems
  if length problems == n && length results == n
    then writeFile (filepath ++ ".out") $ unlines results
    else error $ "number of parsed problems is not matching: " ++ show n ++ " vs " ++ (show . length $ problems)

-- change to represent the problem
type Problem = (Int, Int)

parseProblems :: String -> [Problem]
parseProblems input = case tryParse (everyP problemParser) input of
  Just problems -> problems
  _ -> []

problemParser :: Parser Problem
problemParser = do
  w <- nextNatP
  skipSpacesP
  h <- nextNatP
  skipLineP
  return (w, h)

processProblem :: Problem -> String
processProblem (w, h) = show $ h * div w 3
