{-# LANGUAGE Strict #-}

module Value where

import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Map (Map)
import qualified Data.Map as M
import Control.Exception


newtype ScmNum = ScmNum Integer deriving (Eq, Ord, Read)

instance Show ScmNum where
  show (ScmNum n) = show n


newtype ScmPrim = ScmPrim { runPrimFunc :: [ScmVal] -> IO ScmVal }

instance Eq ScmPrim where
  _ == _ = False


data ScmVal =
    VNil
  | VVoid
  | VChar Char
  | VTrue
  | VFalse
  | VNum ScmNum
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


type Env = [StoreIx]

type StoreIx = Int


newtype Store = Store (Vector Frame)


newtype Frame = Frame (Map String ScmVal)

makeFrame :: [(String, ScmVal)] -> Frame
makeFrame = Frame . M.fromList

searchFrame :: String -> Frame -> Maybe ScmVal
searchFrame s (Frame m) = M.lookup s m

getFrame :: Store -> StoreIx -> Frame
getFrame (Store v) i = (V.!) v i

--- update or insert
updateFrame :: String -> ScmVal -> Frame -> Frame
updateFrame var val (Frame m) = Frame $ M.insert var val m

updateStore :: Store -> StoreIx -> Frame -> Store
updateStore (Store v) i frm = Store $ (V.//) v [(i, frm)]

extendStore :: Store -> (StoreIx, Store)
extendStore (Store v) =
  let i = V.length v
  in (i, Store $ V.snoc v $ Frame M.empty)

initStore :: [(String, ScmVal)] -> Store
initStore = Store . V.singleton . makeFrame

-----------------------------------         Exception

data ScmError =
    InvalidSyntax String
  | NonProcedure ScmVal
  | UnboundVariable String
  | ArityMismatch { expectedArity :: Int, actualArity :: Int, varArity :: Bool }

instance Show ScmError where
  show (InvalidSyntax msg) = msg
  show (UnboundVariable var) = "unbound variable: " ++ var
  show (ArityMismatch m n False)
    | m > n = "too few arguments, expected: " ++ show m ++ ", actual: " ++ show n
    | otherwise = "too many arguments, expected: " ++ show m ++ ", actual: " ++ show n
  show (ArityMismatch m n True) = "too few arguments, expected: " ++ show m ++ "+, actual: " ++ show n
  show (NonProcedure v) = "attempt to apply non-procedure: " ++ show v

instance Exception ScmError


-----------------------      helper functions

isList :: ScmVal -> Bool
isList VNil = True
isList (VCons _ xs) = isList xs
isList _ = False

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

--- works for both lists and dotted lists, the last element is not counted for dotted lists
listLength' :: ScmVal -> Int
listLength' VNil = 0
listLength' (VCons _ xs) = 1 + listLength' xs
listLength' _ = 0

toHsList :: ScmVal -> [ScmVal]
toHsList VNil = []
toHsList (VCons x xs) = x : toHsList xs

fromHsList :: [ScmVal] -> ScmVal
fromHsList [] = VNil
fromHsList (x:xs) = VCons x $ fromHsList xs