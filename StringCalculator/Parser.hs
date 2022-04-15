-- vim:fdm=marker:fmr={{{,}}}
module StringCalculator.Parser where

import           Control.Applicative
import           Data.Bool
import           Data.List
import           Data.Tuple

newtype Parser a = Parser { parse :: String -> Maybe (String, a) }

-- Instances. {{{
instance Functor Parser where
  fmap f (Parser p) = Parser $ \xs -> do
    (rest, a) <- p xs
    return (rest, f a)

instance Applicative Parser where
  pure a = Parser $ \xs -> Just (xs, a)
  (Parser f) <*> (Parser p) = Parser $ \xs -> do
    (xs', f') <- f xs
    (rest, a) <- p xs'
    return (rest, f' a)

instance Monad Parser where
  (Parser p) >>= f = Parser $ \xs -> do
    (rest, c) <- p xs
    parse (f c) rest

instance Alternative Parser where
  empty = Parser $ const Nothing
  (Parser p1) <|> (Parser p2) = Parser $ \xs -> (p1 xs) <|> (p2 xs)
-- }}}

anyCharP :: Parser Char
anyCharP = Parser $ fmap swap . uncons

charP :: Char -> Parser Char
charP c = anyCharP >>= matchCharP
  where
    matchCharP = bool empty . pure <*> (==c)

stringP :: String -> Parser String
stringP = sequenceA . fmap charP

numberP :: Parser Int
numberP = read <$> (zeroP <|> negativeP <|> positiveP)
  where
    zeroP     = stringP "0"
    negativeP = (:) <$> charP '-' <*> positiveP
    positiveP = (:) <$> anyOf ['1'..'9'] <*> many (anyOf ['0'..'9'])
    anyOf     = foldl1 (<|>) . fmap charP
