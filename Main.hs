module Main where

import Control.Applicative (Alternative (empty))
import Control.Monad (when)
import Data.List (maximumBy, nub)
import Data.Maybe (fromJust, isJust, isNothing)
import Debug.Trace (trace)
import Distribution.Simple.Command (OptDescr (BoolOpt))
import MParser
import System.Environment (getArgs)

main :: IO ()
main = main3

cleanNewlines file = filter (/= '\r') file

-- level 1

main1 :: IO ()
main1 = do
  args <- getArgs
  if not (null args)
    then do
      file <- readFile $ head args
      let paths = drop 1 (lines file)
      let res = count paths
      if length res == read (head . lines $ file)
        then writeFile (head args ++ ".out") $ unlines res
        else putStrLn "lines missmatch"
    else putStrLn "too few args"

--
count :: [String] -> [String]
count paths = map cP paths
  where
    cP p = unwords $ map (\d -> show . length $ filter (== d) p) ['W', 'D', 'S', 'A']

-- level 2

main2 :: IO ()
main2 = do
  args <- getArgs
  if not (null args)
    then do
      file <- readFile $ head args
      let paths = drop 1 (lines file)
      let res = map square paths
      if length res == read (head . lines $ file)
        then writeFile (head args ++ ".out") $ unlines res
        else putStrLn "lines missmatch"
    else putStrLn "too few args"

square :: String -> String
square path =
  let (w, h, x, y) = helper path
   in show w ++ " " ++ show h

--                     w h x y
helper :: String -> (Int, Int, Int, Int)
helper p = foldl step (1, 1, 0, 0) p
  where
    step (w, h, x, y) 'A' =
      if x - 1 < 0
        then (w + 1, h, x, y)
        else (w, h, x - 1, y)
    step (w, h, x, y) 'D' =
      if x + 1 >= w
        then (w + 1, h, x + 1, y)
        else (w, h, x + 1, y)
    step (w, h, x, y) 'S' =
      if y - 1 < 0
        then (w, h + 1, x, y)
        else (w, h, x, y - 1)
    step (w, h, x, y) 'W' =
      if y + 1 >= h
        then (w, h + 1, x, y + 1)
        else (w, h, x, y + 1)
    step x _ = x

-- level 3
{-
5 3\n\r
..X..\n\r
.....\n\r
.....\n\r
SSDWDSDWWDSS\n\r
-}

main3 :: IO ()
main3 = do
  args <- getArgs
  if not (null args)
    then do
      file <- cleanNewlines <$> readFile (head args)
      let lawns = parseLawns (unlines . tail . lines $ file)
      let res = map checkValid lawns
      if length res == read (head . lines $ file)
        then writeFile (head args ++ ".out") $ unlines res
        else putStrLn "lines missmatch"
    else putStrLn "too few args"

--           size      tree
--           w    h    x    y    path
type Lawn = (Int, Int, Int, Int, String)

{-
5 3\n\r
..X..\n\r
.....\n\r
.....\n\r
SSDWDSDWWDSS\n\r
-}

parseLawns :: String -> [Lawn]
parseLawns str = case tryParse (everyP lawnP) str of
  Just res -> res
  _ -> []

lawnP :: Parser Lawn
lawnP = do
  w <- natP
  skipSpaces
  h <- natP
  skipLine
  (x, y) <- treeParser w h
  skipWhites
  path <- whileP (\c -> elem c "WASD")
  skipWhites
  return (w, h, x, y, path)
  where
    treeParser :: Int -> Int -> Parser (Int, Int)
    treeParser w h = do
      upper <- whileP (`elem` ".\n\r")
      charP 'X'
      rest <- whileP (== '.')
      skipWhile (`elem` ".\n\r")
      let x = h - length (lines upper)
      let y = w - length rest - 1
      return (x, y)

-- return "VALID" or "INVALID"
checkValid :: Lawn -> String
checkValid (lw, lh, tx, ty, path) =
  let (w, h, x, y) = helper path
      sizeCheck = lw == w && lh == h
      pathCheck = checkPath (w, h) (tx, ty) (x, y) (pathReverse path)
   in if sizeCheck && pathCheck
        then "VALID"
        else "INVALID"

pathReverse :: String -> String
pathReverse = map rev . reverse
  where
    rev 'A' = 'D'
    rev 'D' = 'A'
    rev 'S' = 'W'
    rev 'W' = 'S'
    rev a = a

checkPath :: (Int, Int) -> (Int, Int) -> (Int, Int) -> String -> Bool
checkPath (w, h) tree start path =
  let walk = foldl step [start] path :: [(Int, Int)]
   in maximum (map fst walk) == w - 1
        && maximum (map snd walk) == h - 1 -- size of walk == (w,h)
        && not (elem tree walk)
        && length walk == w * h - 1
        && length walk == length (nub walk) -- no duplicates in walk, could be much faster...
  where
    step :: [(Int, Int)] -> Char -> [(Int, Int)]
    step walk@((x, y) : r) 'W' = (x, y + 1) : walk
    step walk@((x, y) : r) 'S' = (x, y - 1) : walk
    step walk@((x, y) : r) 'A' = (x - 1, y) : walk
    step walk@((x, y) : r) 'D' = (x + 1, y) : walk
    step _ _ = error "wrong path??"

exLawnStr1 = "5 3\n\r..X..\n\r.....\n\r.....\n\rSSDWDSDWWDSS\n\r"

lawn1 = fromJust $ tryParse lawnP exLawnStr2

exLawnStr2 = "5 3\n\r..X..\n\r.....\n\r.....\n\rASSDWDSDWWDSS\n\r"

lawn2 = fromJust $ tryParse lawnP exLawnStr2

path2 = "ASSDWDSDWWDSS"

path2rev = pathReverse path2

check2 = checkPath (5, 3) (2, 2) (4, 0) path2rev

{-

type LawnMap = [((Int,Int),Char)]

checkPath :: (Int,Int) -> (Int,Int) -> (Int,Int) -> String -> Bool
checkPath (w,h) tree start path = --trace (show start) $
    maybe False isFilled (checkPathHelper start path [(start, '.'), (tree, 'X')])
    where
        isFilled :: LawnMap -> Bool
        isFilled lawn = and [isJust $ lookup (x,y) lawn | x<-[0..w-1], y <- [0..h-1]]

checkPathHelper :: (Int,Int) -> String -> LawnMap -> Maybe LawnMap
checkPathHelper (x,y) [] lawn       = Just lawn
checkPathHelper (x,y) (d:rest) lawn = --trace (show (x,y) ++ " " ++ show lawn) $
    let newX = case d of
                'A' -> x-1
                'D' -> x+1
                _   -> x
        newY = case d of
                'W' -> y+1
                'S' -> y-1
                _   -> y
    in  if isNothing $ lookup (newX, newY) lawn
        then checkPathHelper (newX, newY) rest $ ((newX, newY),'.'):lawn
        else Nothing

-- tests

exLawnStr1 = "5 3\n\r..X..\n\r.....\n\r.....\n\rSSDWDSDWWDSS\n\r"
lawn1 = fromJust $ tryParse lawnP exLawnStr2
exLawnStr2 = "5 3\n\r..X..\n\r.....\n\r.....\n\rASSDWDSDWWDSS\n\r"
lawn2 = fromJust $ tryParse lawnP exLawnStr2
path2 = "ASSDWDSDWWDSS"
path2rev = pathReverse path2
check2 = checkPath (5,3) (2,2) (4,0) path2rev

fullLawn2 = checkPathHelper (4,0) path2rev [((4,0), '.'), ((2,2), 'X')]
 -}