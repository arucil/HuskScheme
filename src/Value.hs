{-# LANGUAGE Strict #-}

module Value where

import Data.IORef (IORef)
import Data.Function (on)
import StateT
import Num
import Program (ScmProg)


------------------------           primitive function type        ---------------------

newtype ScmPrim = ScmPrim { runPrimFunc :: [ScmVal] -> StateT FakePtr IO ScmVal }

instance Eq ScmPrim where
  _ == _ = False

------------------------        pointer type      ---------------------

type FakePtr = Int

-----------------------      value type      -----------------------

data ScmVal =
    VNil
  | VVoid
  | VChar Char
  | VTrue
  | VFalse
  | VNum { numValue :: ScmNum }
  | VStr { strPtr :: FakePtr
         , strValue :: String }
  | VSym { symValue :: String }
  | VCons { consPtr :: FakePtr
          , car :: ScmVal
          , cdr :: ScmVal }
  | VClo { closurePtr :: FakePtr
         , closureName :: String
         , closureParams :: [String]
         , closureVararg :: Maybe String
         , closureBody :: [ScmProg]
         , closureEnv :: Env }
  | VPrim { primitivePtr :: FakePtr
          , primitiveName :: String
          , primitiveFunc :: ScmPrim }
  | VMacro { macroName :: String
           , macroParams :: [String]
           , macroVararg :: Maybe String
           , macroBody :: [ScmProg]
           , macroEnv :: Env }

-- corresponds to `eq?` in Scheme
instance Eq ScmVal where
  VNil == VNil = True
  VVoid == VVoid = True
  VChar c == VChar d = c == d
  VTrue == VTrue = True
  VFalse == VFalse = True
  VNum n == VNum m = n == m
  VStr{strPtr = ptr} == VStr{strPtr = ptr'} = ptr == ptr'
  VSym s == VSym t = s == t
  VCons{consPtr = ptr} == VCons{consPtr = ptr'} = ptr == ptr'
  VClo{closurePtr = ptr} == VClo{closurePtr = ptr'} = ptr == ptr'
  VPrim{primitivePtr = ptr} == VPrim{primitivePtr = ptr'} = ptr == ptr'
  _ == _ = False
  
-- Corresponds to `write` function in Scheme
instance Show ScmVal where
  show VNil = "()"
  show VVoid = "#<void>"
  show (VChar '\n') = "#\\newline"
  show (VChar ' ') = "#\\space"
  show (VChar c) = "#\\" ++ [c]
  show VTrue = "#t"
  show VFalse = "#f"
  show (VNum n) = show n

  show (VStr _ s) = "\"" ++ concatMap unescape s ++ "\""
    where
      unescape '\n' = "\\n"
      unescape '\r' = "\\r"
      unescape '\t' = "\\t"
      unescape '"' = "\\\""
      unescape c = [c]

  show (VSym s) = s

  show x@VCons{} = showList' x show

  show (VClo{ closureName = "" }) = "#<procedure>"
  show (VClo{ closureName = name }) = "#<procedure " ++ name ++ ">"
  show (VPrim{ primitiveName = name }) = "#<procedure " ++ name ++ ">"

  show VMacro{ macroName = name } = "#<macro " ++ name ++ ">" -- unreachable

-- Shows lists / dotted lists without enclosing parenthesis
showList' :: ScmVal -> (ScmVal -> String) -> String
showList' (VCons _ a0 d0) f = "(" ++ go a0 d0 ++ ")"
  where
    go a VNil = f a
    go a (VCons _ a' d') = f a ++ " " ++ go a' d'
    go a d = f a ++ " . " ++ f d
showList' _ _ = error "unreachable"


-- Corresponds to `display` function in Scheme
display :: ScmVal -> String
display (VChar c) = [c]
display (VStr _ s) = s
display x@VCons{} = showList' x display
display x = show x


-- Corresponds to `equal?` function in Scheme
infix 4 `equal`
equal :: ScmVal -> ScmVal -> Bool
equal (VStr _ s) (VStr _ s') = s == s'
equal (VCons _ a d) (VCons _ a' d') = equal a a' && equal d d'
equal x y = x == y

-- Corresponds to `eqv?` function in Scheme
infix 4 `eqv`
eqv :: ScmVal -> ScmVal -> Bool
eqv = (==)

------------------------         environment & store


type Env = IORef [Frame]

type Frame = [(String, IORef ScmVal)]


-----------------------      helper functions

isSameType :: ScmVal -> ScmVal -> Bool
isSameType = (==) `on` typeString

typeString :: ScmVal -> String
typeString VNil = "()"
typeString VVoid = "void"
typeString VChar{} = "char"
typeString VTrue = "boolean"
typeString VFalse = "boolean"
typeString VNum{} = "number"
typeString VStr{} = "string"
typeString VSym{} = "symbol"
typeString VCons{} = "cons"
typeString VClo{} = "procedure"
typeString VPrim{} = "procedure"
typeString VMacro{} = "macro"

isProc :: ScmVal -> Bool
isProc VClo{} = True
isProc VPrim{} = True
isProc _ = False

isMacro :: ScmVal -> Bool
isMacro VMacro{} = True
isMacro _ = False

fromBool :: Bool -> ScmVal
fromBool True = VTrue
fromBool False = VFalse


isTrue :: ScmVal -> Bool
isTrue VFalse = False
isTrue _ = True

isList :: ScmVal -> Bool
isList VNil = True
isList (VCons _ _ xs) = isList xs
isList _ = False

toHsList :: ScmVal -> [ScmVal]
toHsList VNil = []
toHsList (VCons _ x xs) = x : toHsList xs
toHsList _ = error "unreachable"