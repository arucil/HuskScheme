{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE Strict #-}

module Eval
  (
    eval
  , evalExpr
  , St
  , initialEnv
  , initialStore
  ) where

import Parse
import StateT
import Value (ScmVal(..), ScmPrim(..), ScmError(..), Env, Store)
import qualified Value as Scm
import Control.Exception (throwIO)
import Control.Monad.State.Class (get, put)
import Control.Applicative ((<|>))
import Control.Monad.IO.Class
import Data.List (foldl', foldl1')


type St = (Env, Store)

type EvalT = StateT St IO


eval :: ScmVal -> St -> IO (ScmVal, St)
eval expr st = runStateT (evalExpr expr) st


evalExpr :: ScmVal -> EvalT ScmVal

evalExpr VTrue = return VTrue
evalExpr VFalse = return VFalse
evalExpr (VChar c) = return $ VChar c
evalExpr (VNum n) = return $ VNum n
evalExpr (VStr s) = return $ VStr s

evalExpr (VCons (VSym "quote")
                (VCons x VNil)) = return x

evalExpr (VCons (VSym "if")
                (VCons test
                       (VCons conseq
                              (VCons alt VNil)))) =
  do
    v <- evalExpr test
    if Scm.isTrue v
      then evalExpr conseq
      else evalExpr alt

--  one-armed if
evalExpr (VCons (VSym "if")
                (VCons test
                       (VCons conseq VNil))) =
  do
    v <- evalExpr test
    if Scm.isTrue v
      then evalExpr conseq
      else return VVoid

evalExpr (VCons (VSym "lambda")
                (VCons params
                       body))
  | not $ Scm.isParamList params = liftIO $ throwIO $ InvalidSyntax $ "invalid parameters: " ++ show params
  | not $ Scm.isList1 body = liftIO $ throwIO $ InvalidSyntax $ "invalid body: " ++ show body
  | otherwise = do
    (env, _) <- get
    return $
      VClo { closureName = ""
           , closureParams = params
           , closureBody = body
           , closureEnv = env
           }

evalExpr (VSym var) = do
  st <- get
  case applyEnv st var of
    Nothing -> liftIO $ throwIO $ UnboundVariable var
    Just v  -> return v

evalExpr (VCons (VSym "set!")
                (VCons (VSym var)
                       (VCons exp VNil))) = do
  st <- get
  case applyEnv st var of
    Nothing -> liftIO $ throwIO $ UnboundVariable var
    _ -> do
      val <- evalExpr exp
      st' <- get
      put $ updateEnv st' var val
      return VVoid

evalExpr (VCons (VSym "define")
                (VCons (VSym var)
                       (VCons exp VNil))) = do
  val <- evalExpr exp
  st <- get
  put $ updateEnv st var val
  return VVoid

evalExpr (VCons (VSym "define")
                (VCons (VCons (VSym var)
                              params)
                       body))
  | not $ Scm.isParamList params = liftIO $ throwIO $ InvalidSyntax $ "invalid parameters: " ++ show params
  | not $ Scm.isList1 body = liftIO $ throwIO $ InvalidSyntax $ "invalid body: " ++ show body
  | otherwise = evalExpr $
    (VCons (VSym "define")
           (VCons (VSym var)
                  (VCons (makeLambda params body)
                         VNil)))

evalExpr (VCons (VSym "begin")
                body)
  | not $ Scm.isList1 body = liftIO $ throwIO $ InvalidSyntax $ "invalid begin body: " ++ show body
  | otherwise = last <$> evalSeq body

evalExpr e@(VCons _ rands)
  | not $ Scm.isList rands = liftIO $ throwIO $ InvalidSyntax $ "invalid function application: " ++ show e
  | otherwise = do
    (rator':rands') <- evalSeq e
    apply rator' rands'

evalExpr e = liftIO $ throwIO $ InvalidSyntax $ "invalid syntax: " ++ show e


evalSeq :: ScmVal -> EvalT [ScmVal]
evalSeq seq = mapM evalExpr $ Scm.toHsList seq

makeLambda :: ScmVal -> ScmVal -> ScmVal
makeLambda params body =
  (VCons (VSym "lambda")
         (VCons params
                body))


apply :: ScmVal -> [ScmVal] -> EvalT ScmVal
apply (VClo{closureParams, closureBody, closureEnv}) args = do
  (env, store) <- get
  (env', store') <- liftIO $ extendEnv closureParams args (closureEnv, store)
  put (env', store')
  v <- last <$> evalSeq closureBody
  (_, store'') <- get
  put (env, store'')
  return v

apply (VPrim{primitiveFunc}) args = liftIO $ runPrimFunc primitiveFunc args

apply v _ = liftIO $ throwIO $ NonProcedure v



applyEnv :: St -> String -> Maybe ScmVal
applyEnv (env, store) var = apply' env var
  where
    apply' :: Env -> String -> Maybe ScmVal
    apply' [] _ = Nothing
    apply' (i:is) s = Scm.searchFrame s (Scm.getFrame store i) <|> apply' is s

--- update or insert
updateEnv :: St -> String -> ScmVal -> St
updateEnv (env@(i:_), store) var val =
  (env, Scm.updateStore store i $
          Scm.updateFrame var (updateProcName val var) $
            Scm.getFrame store i)

extendEnv :: ScmVal -> [ScmVal] -> St -> IO St
extendEnv vars vals (env, store) = do
  bindings <- makeBindings vars vals
  return (i:env, Scm.updateStore store' i $
                   Scm.makeFrame bindings)
  where
    (i, store') = Scm.extendStore store

    makeBindings :: ScmVal -> [ScmVal] -> IO [(String, ScmVal)]
    makeBindings VNil [] = return []
    makeBindings VNil _    = throwIO $ ArityMismatch (Scm.listLength vars) (length vals) False
    makeBindings (VSym var') val' = return [(var', Scm.fromHsList val')]
    makeBindings _ [] = throwIO $ ArityMismatch (Scm.listLength' vars) (length vals) True
    makeBindings (VCons (VSym var') vars') (val':vals') = ((var', updateProcName val' var') :) <$> makeBindings vars' vals'


updateProcName :: ScmVal -> String -> ScmVal
updateProcName clo@(VClo { closureName="" }) name =
  clo { closureName=name }
updateProcName v _ = v


initialEnv :: Env
initialEnv = [0]

initialStore :: Store
initialStore = Scm.initStore $
  [
    ("cons", VPrim "cons" $
      ScmPrim $ \args ->
        assertArgc 2 args $
          return $ VCons (args !! 0) (args !! 1))
  , ("car", VPrim "car" $
      ScmPrim $ \args ->
        assertArgc 1 args $
          assertArgType (VCons VNil VNil) (head args) $
            return $ car (head args))
  , ("cdr", VPrim "cdr" $
      ScmPrim $ \args ->
        assertArgc 1 args $
          assertArgType (VCons VNil VNil) (head args) $
            return $ cdr (head args))
  , ("null?", VPrim "null?" $
      ScmPrim $ \args ->
        assertArgc 1 args $
          return $ Scm.fromBool $ Scm.isSameType VNil (head args))
  , ("+", VPrim "+" $
      ScmPrim $ \args ->
        assertAllArgsType (VNum Scm.zero) args $
          let op (VNum a) (VNum b) = VNum $ a + b
          in return $ foldl' op (VNum 0) args)
  , ("-", VPrim "-" $
      ScmPrim $ \args ->
        assertMoreArgc 1 args $
          assertAllArgsType (VNum Scm.zero) args $
            if length args == 1
              then
                let (VNum n) = head args
                in return $ VNum $ negate n
              else
                let op (VNum a) (VNum b) = VNum $ a - b
                in return $ foldl1' op args)
  , ("*", VPrim "*" $
      ScmPrim $ \args ->
        assertAllArgsType (VNum Scm.zero) args $
          let op (VNum a) (VNum b) = VNum $ a * b
          in return $ foldl' op (VNum 1) args)
  , ("/", VPrim "/" $
      ScmPrim $ \args ->
        assertMoreArgc 1 args $
          assertAllArgsType (VNum Scm.zero) args $
            if length args == 1
              then
                let (VNum n) = head args
                in return $ VNum $ Scm.numRecip n
              else
                let op (VNum a) (VNum b) = VNum $ Scm.numDiv a b
                in return $ foldl1' op args)
  , (">", VPrim ">" $
      ScmPrim $ \args ->
        assertArgc 2 args $
          assertAllArgsType (VNum Scm.zero) args $
            let (VNum a) = head args
                (VNum b) = args !! 1
            in return $ Scm.fromBool $ a > b)
  , (">=", VPrim ">=" $
      ScmPrim $ \args ->
        assertArgc 2 args $
          assertAllArgsType (VNum Scm.zero) args $
            let (VNum a) = head args
                (VNum b) = args !! 1
            in return $ Scm.fromBool $ a >= b)
  , ("<", VPrim "<" $
      ScmPrim $ \args ->
        assertArgc 2 args $
          assertAllArgsType (VNum Scm.zero) args $
            let (VNum a) = head args
                (VNum b) = args !! 1
            in return $ Scm.fromBool $ a < b)
  , ("<=", VPrim "<=" $
      ScmPrim $ \args ->
        assertArgc 2 args $
          assertAllArgsType (VNum Scm.zero) args $
            let (VNum a) = head args
                (VNum b) = args !! 1
            in return $ Scm.fromBool $ a <= b)
  , ("=", VPrim "=" $
      ScmPrim $ \args ->
        assertArgc 2 args $
          assertAllArgsType (VNum Scm.zero) args $
            let (VNum a) = head args
                (VNum b) = args !! 1
            in return $ Scm.fromBool $ a == b)
  , ("print", VPrim "print" $
      ScmPrim $ \args ->
        assertArgc 1 args $
          VVoid <$ (print $ head args))
  ]

assertArgc :: Int -> [ScmVal] -> IO ScmVal -> IO ScmVal
assertArgc n argc ~val =
  if length argc == n
    then val
    else throwIO $ ArityMismatch n (length argc) False

assertMoreArgc :: Int -> [ScmVal] -> IO ScmVal -> IO ScmVal
assertMoreArgc n argc ~val =
  if length argc >= n
    then val
    else throwIO $ ArityMismatch n (length argc) True

assertArgType :: ScmVal -> ScmVal -> IO ScmVal -> IO ScmVal
assertArgType expected actual ~val =
  if Scm.isSameType expected actual
    then val
    else throwIO $ InvalidArgument $ "expected type: " ++ Scm.typeString expected ++ ", actual type: " ++ Scm.typeString actual

assertAllArgsType :: ScmVal -> [ScmVal] -> IO ScmVal -> IO ScmVal
assertAllArgsType expected actuals ~val =
  if all (Scm.isSameType expected) actuals
    then val
    else throwIO $ InvalidArgument $ "expected type: " ++ Scm.typeString expected