{-# LANGUAGE Strict #-}

module Program where

import Data.List (intercalate)
import Num (ScmNum)


data ScmProg =
    PChar Char
  | PTrue
  | PFalse
  | PNum ScmNum
  | PStr String
  | PSym { symProg :: String }
  | PList { listProg :: [ScmProg] }
  | PDList [ScmProg] ScmProg -- dotted list
  deriving Eq

instance Show ScmProg where
  show (PChar ' ') = "#\\space"
  show (PChar '\n') = "#\\newline"
  show (PChar c) = "#\\" ++ [c]
  show PTrue = "#t"
  show PFalse = "#f"
  show (PNum n) = show n
  show (PStr s) = "\"" ++ s ++ "\""
  show (PSym s) = s
  show (PList xs) = "(" ++ intercalate " " (map show xs) ++ ")"
  show (PDList xs x) = "(" ++ concatMap ((++ " ") . show) xs ++ ". " ++ show x ++ ")"


---------------------------       helper functions

-- (define (var . params) body ...)
--         ^^^^^^^^^^^^^^
isDefineHeader :: ScmProg -> Bool
isDefineHeader (PList (PSym _:_)) = True
isDefineHeader (PDList (PSym _:_) _) = True
isDefineHeader _ = False

getDefineName :: ScmProg -> ScmProg
getDefineName (PList (s:_)) = s
getDefineName (PDList (s:_) _) = s
getDefineName _ = error "unreachable"

getDefineParams :: ScmProg -> ScmProg
getDefineParams (PList (_:xs)) = PList xs
getDefineParams (PDList (_:xs) x) = PDList xs x
getDefineParams _ = error "unreachable"


isParamList :: ScmProg -> Bool
isParamList (PList xs) | all isSymbol xs = True
isParamList (PDList xs x) | all isSymbol xs && isSymbol x = True
isParamList (PSym _) = True
isParamList _ = False

--  (arg1 arg2 ...)
--  or
--  (arg1 arg2 ... argk)
--   ^^^^^^^^^^^^^
getParamList :: ScmProg -> [String]
getParamList (PList xs) = map symProg xs
getParamList (PDList xs _) = map symProg xs
getParamList (PSym _) = []
getParamList _ = error "unreachable"

-- get variadic parameter
getParamVar :: ScmProg -> Maybe String
getParamVar (PList _) = Nothing
getParamVar (PDList _ (PSym s)) = Just s
getParamVar (PSym s) = Just s
getParamVar _ = error "unreachable"


isSymbol :: ScmProg -> Bool
isSymbol PSym{} = True
isSymbol _ = False