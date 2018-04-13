{-# LANGUAGE Strict #-}

module Value where

import Data.IORef (IORef)
import Data.Function (on)
import Num
import ContT (ContT(..))
import StateT (StateT(..))


-----------------------     eval type   -----------------------

type Eval a = ContT ScmVal (StateT [EvCont] IO) a

newtype EvCont = EvCont (ScmVal -> StateT [EvCont] IO ScmVal)

------------------------           primitive function type        ---------------------

newtype ScmPrim = ScmPrim { runPrimFunc :: [ScmVal] -> IO ScmVal }

data Op =
    OpApply
  | OpLoad
  | OpEval
  deriving (Eq, Bounded, Enum)

instance Show Op where
  show OpApply = "apply"
  show OpLoad = "load"
  show OpEval = "eval"

-----------------------      value type      -----------------------

data ScmVal =
    VNil
  | VVoid
  | VChar { charValue :: Char }
  | VTrue
  | VFalse
  | VNum { numValue :: ScmNum }
  | VStr { strValue :: String }
  | VSym { symValue :: String }
  | VCons { car :: ScmVal
          , cdr :: ScmVal }
  | VClo { closurePtr :: IORef () -- used for reference equality test
         , closureName :: String
         , closureParams :: Params
         , closureBody :: [ScmVal]
         , closureEnv :: Env }
  | VDelimC { delimContPtr :: IORef ()
            , delimContName :: String
            , delimContVal  :: ScmVal -> StateT [EvCont] IO ScmVal }
  | VPrim { primitivePtr :: IORef () -- used for reference equality test
          , primitiveName :: String
          , primitiveFunc :: ScmPrim }
  | VOp { opVal :: Op }
  | VMacro { macroName :: String
           , macroParams :: Params
           , macroBody :: [ScmVal]
           , macroEnv :: Env }

-- corresponds to `eq?` in Scheme
instance Eq ScmVal where
  VNil == VNil = True
  VVoid == VVoid = True
  VChar c == VChar d = c == d
  VTrue == VTrue = True
  VFalse == VFalse = True
  (VNum n) == (VNum m) = n == m
  (VSym s) == (VSym t) = s == t
  (VStr s) == (VStr t) = s == t
  (VCons a1 d1) == (VCons a2 d2) = a1 == a2 && d1 == d2
  VClo{closurePtr = ptr} == VClo{closurePtr = ptr'} = ptr == ptr'
  (VOp op) == (VOp op') = op == op'
  VPrim{primitivePtr = ptr} == VPrim{primitivePtr = ptr'} = ptr == ptr'
  VDelimC{delimContPtr = ptr} == VDelimC{delimContPtr = ptr'} = ptr == ptr'
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

  show (VStr s) = "\"" ++ concatMap unescape s ++ "\""
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
  show (VOp op) = "#<procedure " ++ show op ++">"

  show (VDelimC{ delimContName = name}) = "#<continuation " ++ name ++ ">"

  show VMacro{ macroName = name } = "#<macro " ++ name ++ ">" -- unreachable


-- Shows lists / dotted lists without enclosing parenthesis
showList' :: ScmVal -> (ScmVal -> String) -> String
showList' (VCons a0 d0) f = "(" ++ go a0 d0 ++ ")"
  where
    go a VNil = f a
    go a (VCons a' d') = f a ++ " " ++ go a' d'
    go a d = f a ++ " . " ++ f d
showList' _ _ = error "unreachable"


-- Corresponds to `display` function in Scheme
display :: ScmVal -> String
display (VChar c) = [c]
display (VStr s) = s
display x@VCons{} = showList' x display
display x = show x


------------------------         closure parameters type

data Params = Params String Params | NoParams | VarParam String

mkParams :: ScmVal -> Params
mkParams VNil = NoParams
mkParams (VCons (VSym s) xs) = Params s $ mkParams xs
mkParams (VSym s) = VarParam s
mkParams _ = error "unreachable"

paramsLength :: Params -> Int
paramsLength NoParams = 0
paramsLength VarParam{} = 0
paramsLength (Params _ xs) = 1 + paramsLength xs

isVariadic :: Params -> Bool
isVariadic (Params _ xs) = isVariadic xs
isVariadic VarParam{} = True
isVariadic _ = False

------------------------         environment & store


type Env = [IORef Frame]

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
typeString VOp{} = "procedure"
typeString VDelimC{} = "procedure"
typeString VMacro{} = "macro"

isProc :: ScmVal -> Bool
isProc VClo{} = True
isProc VPrim{} = True
isProc _ = False

isMacro :: ScmVal -> Bool
isMacro VMacro{} = True
isMacro _ = False

isSymbol :: ScmVal -> Bool
isSymbol VSym{} = True
isSymbol _ = False

fromBool :: Bool -> ScmVal
fromBool True = VTrue
fromBool False = VFalse

isParamList :: ScmVal -> Bool
isParamList VSym{} = True
isParamList (VCons (VSym _) xs) = isParamList xs
isParamList VNil = True
isParamList _ = False

isTrue :: ScmVal -> Bool
isTrue VFalse = False
isTrue _ = True

isList :: ScmVal -> Bool
isList VNil = True
isList (VCons _ xs) = isList xs
isList _ = False

isList1 :: ScmVal -> Bool
isList1 (VCons _ xs) = isList xs
isList1 _ = False

listLength :: ScmVal -> Int
listLength (VCons _ xs) = 1 + listLength xs
listLength _ = 0

toHsList :: ScmVal -> [ScmVal]
toHsList VNil = []
toHsList (VCons x xs) = x : toHsList xs
toHsList _ = error "unreachable"

fromHsList :: [ScmVal] -> ScmVal
fromHsList [] = VNil
fromHsList (x:xs) = VCons x $ fromHsList xs