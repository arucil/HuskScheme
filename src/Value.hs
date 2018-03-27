{-# LANGUAGE Strict #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Value where

import Control.Exception
import Data.Ratio ((%))
import GHC.Real (Ratio((:%)))
import Parser
import Control.Applicative
import Data.IORef
import Data.Function (on)

-------------------------------------    scheme number      ----------------------

newtype ScmNum = ScmNum Rational deriving (Eq, Ord, Num)

instance Show ScmNum where
  show (ScmNum (num :% 1)) = show num
  show (ScmNum (num :% den)) = show num ++ "/" ++ show den

instance Read ScmNum where
  readsPrec _ input =
    case runParser parseNum input of
      Succeed (num, rest) -> [(num, rest)]
      _ -> []
    where
      parseNum :: Parser ScmNum
      parseNum = do
        spaces
        sign  <- maybeOneOf "+-"
        num   <- read <$> some digit
        let num' =
              case sign of 
                Just '-' -> -num
                _        -> num
        slash <- maybeChar '/'
        case slash of
          Nothing -> return $ ScmNum $ num' % 1
          Just _  -> do
            den <- read <$> some digit
            return $ ScmNum $ num' % den

instance Fractional ScmNum where
  ScmNum (num :% den) / ScmNum (num' :% den') = ScmNum $ (num * den') % (den * num')

  fromRational (num :% den) = ScmNum $ num % den

------------------------           primitive function type        ---------------------

newtype ScmPrim = ScmPrim { runPrimFunc :: [ScmVal] -> IO ScmVal }

instance Eq ScmPrim where
  _ == _ = False

---------------------------------------------------------------------------------------


data ScmVal =
    VNil
  | VVoid
  | VChar Char
  | VTrue
  | VFalse
  | VNum { numValue :: ScmNum }
  | VStr String
  | VSym String
  | VCons { car :: ScmVal
          , cdr :: ScmVal }
  | VClo { closureName :: String
         , closureParams :: ScmVal
         , closureBody :: ScmVal
         , closureEnv :: Env }
  | VPrim { primitiveName :: String
          , primitiveFunc :: ScmPrim }
  deriving Eq
  

instance Show ScmVal where
  show VNil = "()"
  show VVoid = "#<void>"
  show (VChar '\n') = "#\\newline"
  show (VChar ' ') = "#\\space"
  show (VChar c) = "#\\" ++ [c]
  show VTrue = "#t"
  show VFalse = "#f"
  show (VNum n) = show n
  show (VStr s) = "\"" ++ s ++ "\""
  show (VSym s) = s

  show (VCons a0 d0) = "(" ++ go a0 d0 ++ ")"
    where
      go a VNil = show a
      go a (VCons a' d') = show a ++ " " ++ go a' d'
      go a d = show a ++ " . " ++ show d

  show (VClo{closureName=""}) = "#<procedure>"
  show (VClo{closureName=name}) = "#<procedure " ++ name ++ ">"
  show (VPrim{primitiveName=name}) = "#<procedure " ++ name ++ ">"


------------------------         environment & store


type Env = IORef [Frame]

type Frame = [(String, IORef ScmVal)]


-----------------------------------         Exception

data ScmError =
    InvalidSyntax String
  | NonProcedure ScmVal
  | UnboundVariable String
  | ArityMismatch { expectedArity :: Int, actualArity :: Int, varArity :: Bool }
  | InvalidArgument String

instance Show ScmError where
  show (InvalidSyntax msg) = msg
  show (UnboundVariable var) = "unbound variable: " ++ var
  show (ArityMismatch m n False)
    | m > n = "too few arguments, expected: " ++ show m ++ ", actual: " ++ show n
    | otherwise = "too many arguments, expected: " ++ show m ++ ", actual: " ++ show n
  show (ArityMismatch m n True) = "too few arguments, expected: " ++ show m ++ "+, actual: " ++ show n
  show (NonProcedure v) = "attempt to apply non-procedure: " ++ show v
  show (InvalidArgument msg) = msg

instance Exception ScmError


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

fromBool :: Bool -> ScmVal
fromBool True = VTrue
fromBool False = VFalse

isList :: ScmVal -> Bool
isList VNil = True
isList (VCons _ xs) = isList xs
isList _ = False

-- is non-empty list
isList1 :: ScmVal -> Bool
isList1 VNil = False
isList1 (VCons _ xs) = isList xs
isList1 _ = False

isTrue :: ScmVal -> Bool
isTrue VFalse = False
isTrue _ = True

isParamList :: ScmVal -> Bool
isParamList VNil = True
isParamList (VSym _) = True
isParamList (VCons (VSym _) xs) = isParamList xs
isParamList _ = False

listLength :: ScmVal -> Int
listLength VNil = 0
listLength (VCons _ xs) = 1 + listLength xs
listLength _ = error "unreachable listLength"

--- works for both lists and dotted lists, the last element is not counted for dotted lists
listLength' :: ScmVal -> Int
listLength' VNil = 0
listLength' (VCons _ xs) = 1 + listLength' xs
listLength' _ = 0

toHsList :: ScmVal -> [ScmVal]
toHsList VNil = []
toHsList (VCons x xs) = x : toHsList xs
toHsList _ = error "unreachable toHsList"

fromHsList :: [ScmVal] -> ScmVal
fromHsList [] = VNil
fromHsList (x:xs) = VCons x $ fromHsList xs