module Prim
  (
    initialEnv
  ) where

import Data.List (foldl', foldl1')
import Data.IORef
import Data.Foldable (fold)
import Control.Monad (when, unless, replicateM)
import Control.Exception (throwIO)
import System.Random (randomRIO)
import Value (Env, ScmVal(..), ScmPrim(..), Op(..))
import qualified Value as V
import Num (ScmNum)
import Error (ScmError(..))


initialEnv :: IO Env
initialEnv = do
  frm1 <- mapM mkPrim primitives
  frm2 <- mapM mkOp ([minBound..maxBound] :: [Op])
  newIORef $ [frm1 ++ frm2]
  where
    mkPrim :: (String, ScmPrim) -> IO (String, IORef ScmVal)
    mkPrim (var, val) = do
      ptr <- newIORef ()
      ref <- newIORef $ VPrim ptr var val
      return (var, ref)

    mkOp :: Op -> IO (String, IORef ScmVal)
    mkOp op = do
      ref <- newIORef $ VOp op
      return (show op, ref)

primitives :: [(String, ScmPrim)]
primitives =
  [
    mkPrim "cons" $ \args -> do
      assertArgc 2 args
      return $ VCons (args !! 0) (args !! 1)

  , mkPrim "car" $ \args -> do
      assertArgc 1 args
      assertArgType (VCons VNil VNil) (head args)
      return $ car (head args)

  , mkPrim "cdr" $ \args -> do
      assertArgc 1 args
      assertArgType (VCons VNil VNil) (head args)
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
      return $ V.fromBool $ V.isSameType (VStr "") $ head args

  , mkPrim "number?" $ \args -> do
      assertArgc 1 args
      return $ V.fromBool $ V.isSameType (VNum 1) $ head args

  , mkPrim "symbol?" $ \args -> do
      assertArgc 1 args
      return $ V.fromBool $ V.isSameType (VSym "") $ head args

  , mkPrim "pair?" $ \args -> do
      assertArgc 1 args
      return $ V.fromBool $ V.isSameType (VCons VNil VNil) $ head args

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
      return $ V.fromBool $ args !! 0 == args !! 1

  , mkPrim "equal?" $ \args -> do
      assertArgc 2 args
      return $ V.fromBool $ args !! 0 == args !! 1

  , mkPrim "display" $ \args -> do
      assertArgc 1 args
      putStr $ V.display $ head args
      return VVoid

  , mkPrim "newline" $ \args -> do
      assertArgc 0 args
      putStrLn ""
      return VVoid

  , mkPrim "write" $ \args -> do
      assertArgc 1 args
      putStr $ show $ head args
      return VVoid

  , mkPrim "print" $ \args -> do
      assertArgc 1 args
      putStrLn $ show $ head args
      return VVoid

  , mkPrim "list" $ \args ->
      return $ V.fromHsList args

  , mkPrim "append" $ \args ->
      if null args
        then return VNil
        else do
          let checkLists [_] = return ()
              checkLists (x:xs) = do
                unless (V.isList x) $
                  throwIO $ InvalidArgument $ "expected list, actual type: " ++ V.typeString x
                checkLists xs
              checkLists _ = error "unreachable"
          checkLists args
          return $ foldr VCons (last args) $ fold $ map V.toHsList $ init args

  , mkPrim "length" $ \args -> do
      assertArgc 1 args
      let arg1 = head args
      unless (V.isList arg1) $
        throwIO $ InvalidArgument $ "expected list, actual type: " ++ V.typeString arg1
      return $ VNum $ fromIntegral $ V.listLength arg1

  , mkPrim "error" $ \args -> do
      assertArgc 1 args
      assertArgType (VStr "") (head args)
      throwIO $ CustomError $ strValue $ head args

  , mkPrim "gensym" $ \args -> do
      assertMoreArgc 0 args
      when (length args == 1) $
        assertArgType (VSym "") (head args)
      let chars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']
      sym <- ("SYM." ++) . map (chars !!) <$> replicateM 16 (randomRIO (0, length chars - 1))
      if length args == 1
        then return $ VSym $ symValue (head args) ++ sym
        else return $ VSym sym
  ]
  where
    mkPrim :: String -> ([ScmVal] -> IO ScmVal) -> (String, ScmPrim)
    mkPrim name f = (name, ScmPrim f)

    mkNumCmp :: String -> (ScmNum -> ScmNum -> Bool) -> (String, ScmPrim)
    mkNumCmp name op = mkPrim name $ \args -> do
      assertArgc 2 args
      assertAllArgTypes (VNum 1) args
      let (VNum a) = args !! 0
          (VNum b) = args !! 1
      return $ V.fromBool $ a `op` b


assertArgc :: Int -> [ScmVal] -> IO ()
assertArgc n args =
  unless (length args == n) $
    throwIO $ ArityMismatch n (length args) False

assertMoreArgc :: Int -> [ScmVal] -> IO ()
assertMoreArgc n args =
  unless (length args >= n) $
    throwIO $ ArityMismatch n (length args) True

assertArgType :: ScmVal -> ScmVal -> IO ()
assertArgType expected actual =
  unless (V.isSameType expected actual) $
    throwIO $ InvalidArgument $ "expected type: " ++ V.typeString expected ++ ", actual type: " ++ V.typeString actual

assertAllArgTypes :: ScmVal -> [ScmVal] -> IO ()
assertAllArgTypes expected actuals =
  unless (all (V.isSameType expected) actuals) $
    throwIO $ InvalidArgument $ "expected type: " ++ V.typeString expected