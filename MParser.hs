{-
Author: Yannik Eikmeier
-}
module MParser (module MParser) where


import Control.Applicative
import Control.Monad.State
import GHC.Base (VecElem(Int16ElemRep))
import GHC.Exts.Heap (GenClosure(DoubleClosure))

------------------
-- USAGE
{-
  Monadic Parser Implementation
  import and use functions to build parser

-}
------------------

newtype Parser a = Parser { parse :: String -> [(a,String)]}

newParser :: (String -> [(a,String)]) -> Parser a
newParser = Parser

tryParse :: Parser a -> String -> Maybe a
tryParse p s = case filter (null.snd) $ parse p s of
              (x,_):_ -> Just x
              _       -> Nothing

noParse = []

-- pure/return
yield :: a -> Parser a
yield x = Parser $ \s -> [(x,s)]

instance Functor Parser where
  fmap f p = yield f <*> p

instance Applicative Parser where
  pure = yield
  p <*> q = Parser $ \s -> [ (f x,s2) | (f,s1) <- parse p s,
                                        (x,s2) <- parse q s1 ]


instance Monad Parser where
  return = pure
  p >>= f = Parser $ \s -> [(y,s2)| (x,s1) <- parse p s,
                                    (y,s2) <- parse (f x) s1]

failP :: Parser a
failP = Parser $ const noParse

instance Alternative Parser where
  empty   = failP
  p <|> q = Parser $ \s -> parse p s ++ parse q s


infixl 3 <!>
(<!>) :: Parser a -> Parser a -> Parser a
p <!> q = Parser $ \s -> case parse p s of
                              [] -> parse q s
                              xs -> xs

--------------------------------
-- combiners
--------------------------------

-- parse or default value
maybeP :: a -> Parser a -> Parser a
maybeP a p = p <!> yield a

-- some but not all, non deterministic!
manyP :: Parser a -> Parser [a]
manyP p = (:) <$> p <*> manyP p <|> yield []

-- all that match
everyP :: Parser a -> Parser [a]
everyP p = (:) <$> p <*> everyP p <!> yield []


--------------------------------
-- basics
--------------------------------

idP :: Parser ()
idP = yield ()

checkP :: (a->Bool) -> Parser a -> Parser a
checkP ok p = Parser $ filter (ok . fst) . parse p

whileP :: (Char -> Bool) -> Parser String
whileP ok = (:) <$> checkP ok nextCharP <*> whileP ok
            <!> yield ""


charP :: Char -> Parser Char
charP x =  Parser help
  where help (c:cs) | x == c = [('c',cs)]
        help _               = []

charP_ :: Char -> Parser ()
charP_ c = charP c *> idP

stringP :: String -> Parser ()
stringP = mapM_ charP
{- stringP []     = idP
stringP (c:cs) = charP c *> stringP cs
 -}


-- Skippers
----------------

skipP :: Int -> Parser ()
skipP n = replicateM_ n nextCharP

skipWhileP :: (Char -> Bool) -> Parser ()
skipWhileP ok = checkP ok nextCharP *> skipWhileP ok
               <!> idP

skipSpacesP :: Parser ()
skipSpacesP = everyP (charP ' ') *> idP

skipBreakP :: Parser ()
skipBreakP = everyP (charP '\r') *> charP '\n' *> idP

skipWhitesP :: Parser ()
skipWhitesP = skipWhileP (`elem` " \r\n")
-- not working?
-- skipWhitesP = everyP (skipSpacesP <!> skipBreakP) *> idP

skipLineP :: Parser ()
skipLineP = skipWhileP (`notElem` "\r\n") *> skipBreakP
--skipLineP = skipWhile (\c -> c /= '\n') *> skip 1 *> everyP (charP '\r') *> idP



--------------------------------
-- readers
--------------------------------

nextCharP :: Parser Char
nextCharP = Parser help
  where help []     = []
        help (c:cs) = [(c,cs)]

nextNCharP :: Int -> Parser String
nextNCharP 0 = yield []
nextNCharP n = replicateM n nextCharP

nextWordP :: Parser String
nextWordP = skipWhitesP *> whileP (`notElem` " \r\n")

nextLineP :: Parser String
nextLineP = do
  line <- whileP (`notElem` "\r\n")
  skipBreakP
  return line



--------------------------------
-- numbers
--------------------------------


nextDigitP :: Parser Int
nextDigitP = charP '0' *> yield 0
         <!> charP '1' *> yield 1
         <!> charP '2' *> yield 2
         <!> charP '3' *> yield 3
         <!> charP '4' *> yield 4
         <!> charP '5' *> yield 5
         <!> charP '6' *> yield 6
         <!> charP '7' *> yield 7
         <!> charP '8' *> yield 8
         <!> charP '9' *> yield 9

nextNatP :: Parser Int
nextNatP = read <$> whileP (`elem` "0123456789")
{- natP = help <$> checkP (not.null) (everyP digitP)
  where
  help ds = let size = length ds - 1 in
    foldl (\m (i,n) -> (10^i)*n+m ) 0 (zip [size,size-1..0] ds)
 -}

nextSignP :: Parser Int
nextSignP = (charP '-' *> yield (-1))
            <!>  (charP '+' *>  yield 1)
            <!>  yield 1 
{- nextSignP = (charP '-' *> nextSignP >>= \n -> return (negate n))
       <!>  charP '+' *> nextSignP
       <!>  yield 1
 -}
nextIntP :: Parser Int
nextIntP = (*) <$> nextSignP <*> nextNatP

nextDoubleP :: Parser Double
nextDoubleP = do
  s <- nextSignP
  d <- whileP (`elem` ".0123456789")
  return $ read d * fromIntegral s
--doubleP = (*) <$> signP <*> ((read::String -> Double) <$> whileP (`elem` ".0123456789"))



-- others
----------


palindromeP :: Parser ()
palindromeP = do
  u <- many nextCharP
  idP <|> (nextCharP *> idP)
  stringP (reverse u)