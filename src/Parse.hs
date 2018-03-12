module Parse
  (
    parse
  ) where

import Parser
import Value
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
        '('  -> return LParen
        ')'  -> return RParen
        '['  -> return LBrack
        ']'  -> return RBrack
        '"'  -> TokStr <$> tokenString
        '#'  -> tokenHash
        ';'  -> skipLine >> getToken
        x | elem x identChars -> tokenSymOrNum x
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
      if x == '\\'
        then do
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
        else do
          xs <- many letter
          case (x:xs) of 
            "t" -> return TokT
            "f" -> return TokF
            s   -> fail $ "invalid token #" ++ s

    tokenSymOrNum :: Char -> Parser Token
    tokenSymOrNum x = do
      xs <- many $ oneOf identChars
      case (x:xs) of
        "." -> return Dot
        s   -> return $ maybe (TokSym s) (TokNum . ScmNum) $ readMaybe s

          
parse :: Parser ScmVal
parse = do
  tok <- getToken
  exp <- parseExpr tok
  tok' <- getToken
  if tok' == EOF
    then return exp
    else fail $ "expected: EOF, got: " ++ show tok'

parseExpr :: Token -> Parser ScmVal
parseExpr tok =
  case tok of
    TokT      -> return VTrue
    TokF      -> return VFalse
    TokNum n  -> return $ VNum n
    TokStr s  -> return $ VStr s
    TokSym s  -> return $ VSym s
    TokChar c -> return $ VChar c
    Quote     -> do
      exp  <- getToken >>= parseExpr
      return $
        VCons (VSym "quote")
              (VCons exp VNil)
    LParen    -> parseList RParen
    LBrack    -> parseList RBrack
    _         -> fail $ "invalid token " ++ show tok
  where
    parseList :: Token -> Parser ScmVal
    parseList rparen = do
      tok <- getToken
      case tok of
        Dot -> do
          exp <- getToken >>= parseExpr
          tok' <- getToken
          if tok' == rparen
            then return exp
            else fail $ "expected: " ++ show rparen ++ ", got: " ++ show tok'
        _ | tok == rparen -> return VNil
        _   -> VCons <$> parseExpr tok <*> parseList rparen