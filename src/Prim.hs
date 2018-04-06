module Prim
  (
    initialEnv
  ) where

import Data.List (foldl', foldl1')
import Data.IORef
import Control.Monad.IO.Class
import Control.Monad (unless)
import Eval (Eval, newPtr, apply, throwE)
import Value (Env, ScmVal(..), ScmPrim(..))
import qualified Value as V
import Num (ScmNum)
import Error (ScmError(..))


initialEnv :: Eval Env
initialEnv = do
  frm <- mapM mkBinding initialBindings
  liftIO $ newIORef [frm]
  where
    mkBinding :: (String, ScmVal) -> Eval (String, IORef ScmVal)
    mkBinding (var, val@VPrim{}) = do
      ptr <- newPtr
      ref <- liftIO $ newIORef $ val { primitivePtr = ptr }
      return (var, ref)
    mkBinding (var, val) = liftIO $ (,) var <$> newIORef val

initialBindings :: [(String, ScmVal)]
initialBindings =
  [
    mkPrim "cons" $ \args -> do
      assertArgc 2 args
      ptr <- newPtr
      return $ VCons ptr (args !! 0) (args !! 1)

  , mkPrim "car" $ \args -> do
      assertArgc 1 args
      assertArgType (VCons 0 VNil VNil) (head args)
      return $ car (head args)

  , mkPrim "cdr" $ \args -> do
      assertArgc 1 args
      assertArgType (VCons 0 VNil VNil) (head args)
      return $ cdr (head args)


  , mkPrim "null?" $ \args -> do
      assertArgc 1 args
      return $ V.fromBool $ V.isSameType VNil $ head args

  , mkPrim "boolean?" $ \args -> do
      assertArgc 1 args
      return $ V.fromBool $ V.isSameType VTrue $ head args

  , mkPrim "char?" $ \args -> do
      assertArgc 1 args
      return $ V.fromBool $ V.isSameType (VChar ' ') $ head args

  , mkPrim "string?" $ \args -> do
      assertArgc 1 args
      return $ V.fromBool $ V.isSameType (VStr 0 "") $ head args

  , mkPrim "number?" $ \args -> do
      assertArgc 1 args
      return $ V.fromBool $ V.isSameType (VNum 1) $ head args

  , mkPrim "symbol?" $ \args -> do
      assertArgc 1 args
      return $ V.fromBool $ V.isSameType (VSym "") $ head args

  , mkPrim "pair?" $ \args -> do
      assertArgc 1 args
      return $ V.fromBool $ V.isSameType (VCons 0 VNil VNil) $ head args

  , mkPrim "procedure?" $ \args -> do
      assertArgc 1 args
      return $ V.fromBool $ V.isProc $ head args


  , mkPrim "+" $ \args -> do
      assertAllArgTypes (VNum 1) args
      return $ VNum $ foldl' (+) 0 $ map numValue args

  , mkPrim "*" $ \args -> do
      assertAllArgTypes (VNum 1) args
      return $ VNum $ foldl' (*) 1 $ map numValue args

  , mkPrim "-" $ \args -> do
      assertMoreArgc 1 args
      assertAllArgTypes (VNum 1) args
      if length args == 1
        then return $ VNum $ negate $ numValue $ head args
        else return $ VNum $ foldl1' (-) $ map numValue args

  , mkPrim "/" $ \args -> do
      assertMoreArgc 1 args
      assertAllArgTypes (VNum 1) args
      if length args == 1
        then return $ VNum $ recip $ numValue $ head args
        else return $ VNum $ foldl1' (/) $ map numValue args

  , mkNumCmp ">" (>)
  , mkNumCmp ">=" (>=)
  , mkNumCmp "<" (<)
  , mkNumCmp "<=" (<=)
  , mkNumCmp "=" (==)

  , mkPrim "eq?" $ \args -> do
      assertArgc 2 args
      return $ V.fromBool $ args !! 0 == args !! 1

  , mkPrim "eqv?" $ \args -> do
      assertArgc 2 args
      return $ V.fromBool $ args !! 0 `V.eqv` args !! 1

  , mkPrim "equal?" $ \args -> do
      assertArgc 2 args
      return $ V.fromBool $ args !! 0 `V.equal` args !! 1

  , mkPrim "display" $ \args -> do
      assertArgc 1 args
      liftIO $ putStr $ V.display $ head args
      return VVoid

  , mkPrim "newline" $ \args -> do
      assertArgc 0 args
      liftIO $ putStrLn ""
      return VVoid

  , mkPrim "write" $ \args -> do
      assertArgc 1 args
      liftIO $ putStr $ show $ head args
      return VVoid

  , mkPrim "print" $ \args -> do
      assertArgc 1 args
      liftIO $ putStrLn $ show $ head args
      return VVoid

  , mkPrim "apply" $ \args -> do
      assertMoreArgc 2 args
      let fn = head args
          args' = tail args
          last'  = last args'
      if V.isList last'
        then apply fn $ init args' ++ V.toHsList last'
        else throwE $ InvalidArgument $ "expected list, actual type: " ++ V.typeString last'

  ]
  where
    mkPrim :: String -> ([ScmVal] -> Eval ScmVal) -> (String, ScmVal)
    mkPrim name f = (name, VPrim 0 name $ ScmPrim f)

    mkNumCmp :: String -> (ScmNum -> ScmNum -> Bool) -> (String, ScmVal)
    mkNumCmp name op = mkPrim name $ \args -> do
      assertArgc 2 args
      assertAllArgTypes (VNum 1) args
      let (VNum a) = args !! 0
          (VNum b) = args !! 1
      return $ V.fromBool $ a `op` b


assertArgc :: Int -> [ScmVal] -> Eval ()
assertArgc n argc =
  unless (length argc == n) $
    throwE $ ArityMismatch n (length argc) False

assertMoreArgc :: Int -> [ScmVal] -> Eval ()
assertMoreArgc n argc =
  unless (length argc >= n) $
    throwE $ ArityMismatch n (length argc) True

assertArgType :: ScmVal -> ScmVal -> Eval ()
assertArgType expected actual =
  unless (V.isSameType expected actual) $
    throwE $ InvalidArgument $ "expected type: " ++ V.typeString expected ++ ", actual type: " ++ V.typeString actual

assertAllArgTypes :: ScmVal -> [ScmVal] -> Eval ()
assertAllArgTypes expected actuals =
  unless (all (V.isSameType expected) actuals) $
    throwE $ InvalidArgument $ "expected type: " ++ V.typeString expected