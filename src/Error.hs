module Error where

import Control.Exception
import Value


data ScmError =
    InvalidSyntax String
  | NonProcedure ScmVal
  | UnboundVariable String
  | ArityMismatch { expectedArity :: Int, actualArity :: Int, varArity :: Bool }
  | InvalidArgument String
  | CustomError String

instance Show ScmError where
  show (InvalidSyntax msg) = msg
  show (UnboundVariable var) = "unbound variable: " ++ var
  show (ArityMismatch m n False)
    | m > n = "too few arguments, expected: " ++ show m ++ ", actual: " ++ show n
    | otherwise = "too many arguments, expected: " ++ show m ++ ", actual: " ++ show n
  show (ArityMismatch m n True) = "too few arguments, expected: " ++ show m ++ "+, actual: " ++ show n
  show (NonProcedure v) = "attempt to apply non-procedure: " ++ show v
  show (InvalidArgument msg) = msg
  show (CustomError msg) = msg

instance Exception ScmError