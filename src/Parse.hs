module Parse
  (
    parse
  , parseList
  ) where

import Parser
import Value (ScmVal(..))
import Num(ScmNum)
import Control.Applicative
import Data.Char (isLetter)
import Text.Read (readMaybe)


data Token =
    TokT
  | TokF
  | TokNum ScmNum
  | TokChar Char
  | TokStr String
  | TokSym String
  | LParen
  | RParen
  | LBrack
  | RBrack
  | Dot
  | EOF
  | Quote
  | Backtick
  | Comma
  | CommaAt
  deriving (Eq, Show)


identChars :: [Char]
identChars = ['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z'] ++ "!$%&*+-./:<=>?@^_~"

getToken :: Parser Token
getToken = do
  spaces
  EOF <$ eof
   <|>
    do
      c <- anyChar
      case c of
        '\'' -> return Quote
        '`'  -> return Backtick
        ','  -> tokenComma
        '('  -> return LParen
        ')'  -> return RParen
        '['  -> return LBrack
        ']'  -> return RBrack
        '"'  -> TokStr <$> tokenString
        '#'  -> tokenHash
        ';'  -> skipLine >> getToken
        x | elem x identChars -> tokenSymOrNum x
        _    -> fail $ "invalid character '" ++ [c] ++ "'"
  where
    tokenString :: Parser String
    tokenString = do
      c <- anyChar
      case c of
        '"'  -> return []
        '\\' ->
          do
            c' <- anyChar
            case c' of
              'n' -> ('\n':) <$> tokenString
              't' -> ('\t':) <$> tokenString
              'r' -> ('\r':) <$> tokenString
              '"' -> ('"':)  <$> tokenString
              _   -> fail $ "invalid escape sequence \\" ++ [c']
        _ -> (c:) <$> tokenString

    tokenHash :: Parser Token
    tokenHash = do
      x <- anyChar
      case x of
        '\\' -> do
          x' <- anyChar
          if isLetter x'
            then do
              xs <- many letter
              case (x':xs) of
                "newline" -> return $ TokChar '\n'
                "space"   -> return $ TokChar ' '
                [c]       -> return $ TokChar c
                s         -> fail $ "invalid character #\\" ++ s
            else return $ TokChar x'
        _ | isLetter x -> do
          xs <- many letter
          case (x:xs) of 
            "t" -> return TokT
            "f" -> return TokF
            s   -> fail $ "invalid token #" ++ s
        _ -> fail $ "invalid token #" ++ [x]

    tokenSymOrNum :: Char -> Parser Token
    tokenSymOrNum x = do
      xs <- many $ oneOf identChars
      case (x:xs) of
        "." -> return Dot
        s   -> return $ maybe (TokSym s) TokNum $ readMaybe s

    tokenComma :: Parser Token
    tokenComma = do
      c <- maybeChar '@'
      case c of
        Nothing -> return Comma
        Just _  -> return CommaAt

          
parse :: Parser ScmVal
parse = do
  expr <- parse'
  tok <- getToken
  if tok == EOF
    then return expr
    else fail $ "expected: EOF, got: " ++ show tok

parse' :: Parser ScmVal
parse' = parseExpr

parseList :: Parser [ScmVal]
parseList = do
  exps <- many parse'
  tok <- getToken
  if tok == EOF
    then return exps
    else fail $ "expected: EOF, got: " ++ show tok

parseExpr :: Parser ScmVal
parseExpr = do
  tok <- getToken
  case tok of
    TokT      -> return VTrue
    TokF      -> return VFalse
    TokNum n  -> return $ VNum n
    TokStr s  -> return $ VStr s
    TokSym s  -> return $ VSym s
    TokChar c -> return $ VChar c
    Quote     -> quote "quote"
    Backtick  -> quote "quasiquote"
    Comma     -> quote "unquote"
    CommaAt   -> quote "unquote-splicing"
    LParen    -> list RParen
    LBrack    -> list RBrack
    _         -> fail $ "invalid token " ++ show tok
  where
    quote :: String -> Parser ScmVal
    quote q = do
      expr <- parseExpr
      return $
        VCons (VSym q)
              (VCons expr VNil)

    list :: Token -> Parser ScmVal
    list rparen = do
      exps <- many parseExpr
      tok' <- getToken
      case tok' of
        Dot | not $ null exps -> do
          exp1 <- parseExpr
          tok'' <- getToken
          if tok'' == rparen
            then return $ foldr VCons exp1 exps
            else fail $ "expected: " ++ show rparen ++ ", got: " ++ show tok''
        _ | tok' == rparen -> return $ foldr VCons VNil exps
        _ -> fail $ "expected: " ++ show rparen ++ ", got: " ++ show tok'