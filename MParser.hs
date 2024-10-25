{-
Author: Yannik Eikmeier
-}
module MParser
  ( Parser (parse),
    newParser,
    tryParse,
    yield,
    failP,
    (<!>),
    idP,
    skip,
    skipSpaces,
    skipWhites,
    skipWhile,
    skipLine,
    whileP,
    charP,
    stringP,
    nextCharP,
    nextNCharP,
    nextWordP,
    digitP,
    signP,
    intP,
    natP,
    checkP,
    manyP,
    everyP,
    palindromeP,
  )
where

import Control.Applicative
import Control.Monad.State

------------------
-- USAGE
{-
  Monadic Parser Implementation
  import and use functions to build parser

-}
------------------

newtype Parser a = Parser {parse :: String -> [(a, String)]}

newParser :: (String -> [(a, String)]) -> Parser a
newParser = Parser

tryParse :: Parser a -> String -> Maybe a
tryParse p s = case filter (null . snd) $ parse p s of
  (x, _) : _ -> Just x
  _ -> Nothing

noParse = []

-- pure/return
yield :: a -> Parser a
yield x = Parser $ \s -> [(x, s)]

instance Functor Parser where
  fmap f p = yield f <*> p

instance Applicative Parser where
  pure = yield
  p <*> q = Parser $ \s ->
    [ (f x, s2) | (f, s1) <- parse p s, (x, s2) <- parse q s1
    ]

instance Monad Parser where
  return = pure
  p >>= f = Parser $ \s ->
    [ (y, s2) | (x, s1) <- parse p s, (y, s2) <- parse (f x) s1
    ]

failP :: Parser a
failP = Parser $ const noParse

instance Alternative Parser where
  empty = failP
  p <|> q = Parser $ \s -> parse p s ++ parse q s

infixl 3 <!>

(<!>) :: Parser a -> Parser a -> Parser a
p <!> q = Parser $ \s -> case parse p s of
  [] -> parse q s
  xs -> xs

--------------------------------
-- examples and usefull stuff
--------------------------------

idP :: Parser ()
idP = yield ()

skip :: Int -> Parser ()
skip n = replicateM_ n nextCharP

skipWhile :: (Char -> Bool) -> Parser ()
skipWhile ok =
  (checkP ok nextCharP)
    *> (skipWhile ok)
      <!> idP

skipSpaces :: Parser ()
skipSpaces = everyP (charP ' ') *> idP

skipWhites :: Parser ()
skipWhites = everyP (charP ' ' <!> stringP "\n\r" <!> charP '\n') *> idP

skipLine :: Parser ()
skipLine = skipWhile (\c -> c /= '\n') *> skip 1 *> everyP (charP '\r') *> idP

charP :: Char -> Parser ()
charP x = Parser help
  where
    help (c : cs) | x == c = [((), cs)]
    help _ = []

nextCharP :: Parser Char
nextCharP = Parser help
  where
    help [] = []
    help (c : cs) = [(c, cs)]

nextNCharP :: Int -> Parser String
nextNCharP 0 = yield []
nextNCharP n = replicateM n nextCharP

whileP :: (Char -> Bool) -> Parser String
whileP ok =
  (:)
    <$> (checkP ok nextCharP)
    <*> (whileP ok)
      <!> yield ""

nextWordP :: Parser String
nextWordP = whileP (\c -> c /= ' ' && c /= '\n')

nextLineP :: Parser String
nextLineP = do
  line <- whileP (\c -> c /= '\n')
  skip 1
  everyP (charP '\r')
  return line

stringP :: String -> Parser ()
stringP [] = idP
stringP (c : cs) = charP c *> stringP cs

digitP :: Parser Int
digitP =
  charP '0'
    *> yield 0
      <!> charP '1'
    *> yield 1
      <!> charP '2'
    *> yield 2
      <!> charP '3'
    *> yield 3
      <!> charP '4'
    *> yield 4
      <!> charP '5'
    *> yield 5
      <!> charP '6'
    *> yield 6
      <!> charP '7'
    *> yield 7
      <!> charP '8'
    *> yield 8
      <!> charP '9'
    *> yield 9

natP :: Parser Int
natP = read <$> whileP (\c -> elem c "0123456789")

{- natP = help <$> checkP (not.null) (everyP digitP)
  where
  help ds = let size = length ds - 1 in
    foldl (\m (i,n) -> (10^i)*n+m ) 0 (zip [size,size-1..0] ds)
 -}

signP :: Parser Int
signP =
  (charP '-' *> signP >>= \n -> return (0 - n))
    <!> charP '+'
    *> signP
      <!> yield 1

intP :: Parser Int
intP = (*) <$> signP <*> natP

checkP :: (a -> Bool) -> Parser a -> Parser a
checkP ok p = Parser $ filter (ok . fst) . (parse p)

manyP :: Parser a -> Parser [a]
manyP p = (:) <$> p <*> manyP p <|> yield []

everyP :: Parser a -> Parser [a]
everyP p = (:) <$> p <*> everyP p <!> yield []

palindromeP :: Parser ()
palindromeP = do
  u <- many nextCharP
  (idP <|> (nextCharP *> idP))
  stringP (reverse u)
