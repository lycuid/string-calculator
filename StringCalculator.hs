module StringCalculator (stringCalc, CalcError(..)) where

import           Control.Applicative
import           StringCalculator.Parser

data CalcError  = MalformedString String
                | NegativeNumbers [Int]
  deriving (Eq, Show)

delimiter :: Parser Char
delimiter = (stringP "//" *> anyCharP <* stringP "\n") <|> pure ','

numbers :: Char -> Parser [Int]
numbers delim = (:) <$> (numberP <|> pure 0) <*> many (delimP *> numberP)
  where
    delimP = charP delim <|> charP '\n'

stringCalc :: String -> Either CalcError Int
stringCalc input =
  case parse (delimiter >>= numbers) input of
    Nothing       -> Left $ MalformedString input
    Just ([], xs) -> case length ns of
                        0 -> Right (sum xs)
                        _ -> Left $ NegativeNumbers ns
                        where
                          ns = filter (<0) xs
    Just (xs, _)  -> Left $ MalformedString xs
