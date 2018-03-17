module Parser
  (
    Result(..)
  , Parser(..)
  , (<?>)
  , anyChar
  , eof
  , maybeChar
  , maybeOneOf
  , oneOf
  , letter
  , digit
  , spaces
  , token
  , newline
  , skipLine
  ) where

import Control.Applicative


data Result a = Fail String | Succeed a deriving (Eq, Show)

instance Functor Result where
  fmap _ (Fail e) = Fail e
  fmap f (Succeed a) = Succeed (f a)

instance Applicative Result where
  pure = Succeed

  Fail e <*> _ = Fail e
  _ <*> Fail e = Fail e
  Succeed f <*> Succeed a = Succeed $ f a

instance Monad Result where
  Fail e >>= _ = Fail e
  Succeed a >>= f = f a


newtype Parser a = P { runParser :: String -> Result (a, String) }

instance Functor Parser where
  fmap f (P p) = P (\s ->
    case p s of
      Fail e -> Fail e
      Succeed (a, s') -> Succeed (f a, s'))

instance Applicative Parser where
  pure a = P (\s -> Succeed (a, s))

  P p <*> a = P (\s ->
    case p s of
      Fail e -> Fail e
      Succeed (f, s') -> runParser (fmap f a) s')

instance Monad Parser where
  P pa >>= f = P (\s ->
    case pa s of
      Fail e -> Fail e
      Succeed (a, s') -> runParser (f a) s')

  fail e = P (\_ -> Fail e)


instance Alternative Parser where
  empty = fail "no choices"

  p <|> q = P (\s ->
    case runParser p s of
      Fail _ -> runParser q s
      x -> x)

-- give the parser a name
infixl 3 <?>
(<?>) :: Parser a -> String -> Parser a
p <?> name = P (\s ->
  case runParser p s of
    Fail _ -> Fail $ "expected: " ++ name
    x -> x)


expect :: String -> String -> String
expect a b = "expected: " ++ a ++ ", got: " ++ b

-----------------------                   parsers


anyChar :: Parser Char
anyChar = P (\s ->
  case s of
    [] -> Fail $ expect "a char" "EOF"
    (x:xs) -> Succeed (x, xs))

eof :: Parser ()
eof = P (\s ->
  case s of
    [] -> Succeed ((), [])
    (x:_) -> Fail $ expect "EOF" [x])

maybeChar :: Char -> Parser (Maybe Char)
maybeChar c = P (\s ->
  case s of
    (x:xs) | x == c -> Succeed (Just x, xs)
    _     -> Succeed (Nothing, s))

maybeOneOf :: [Char] -> Parser (Maybe Char)
maybeOneOf cs = P (\s ->
  case s of
    (x:xs) | elem x cs -> Succeed (Just x, xs)
    _                  -> Succeed (Nothing, s))

char :: Char -> Parser Char
char c = do
  x <- anyChar
  if x == c
    then return x
    else fail $ expect [c] [x]


oneOf :: [Char] -> Parser Char
oneOf xs = do
  x <- anyChar
  if elem x xs
    then return x
    else fail $ expect ("oneOf \"" ++ xs ++ "\"") [x]

letter :: Parser Char
letter = oneOf $ ['a'..'z'] ++ ['A'..'Z']

digit :: Parser Char
digit = oneOf ['0'..'9']

spaces :: Parser ()
spaces = () <$ many (oneOf " \t\n\r")

-- convert a parser to another one which will discard leading spaces
token :: Parser a -> Parser a
token = (spaces >>)

newline :: Parser ()
newline = () <$ char '\n'

-- consumes EOL
skipLine :: Parser ()
skipLine = P (\s ->
  Succeed ((),
    case dropWhile (/= '\n') s of
      [] -> []
      (_:s') -> s'))