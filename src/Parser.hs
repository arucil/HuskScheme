module Parser
  where

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