{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}

module Eval
  (
    eval
  , runEval
  , throwE
  , loadFile
  , getVariables
  ) where

import Prelude hiding (exp)
import Control.Exception (throwIO, catch, Exception, SomeException(..))
import Control.Applicative ((<|>))
import Control.Monad.IO.Class (liftIO)
import Control.Monad (unless)
import Data.List (nub)
import Data.IORef
import Value (ScmVal(..), ScmPrim(..), Env, Frame, Params(..), Eval, EvCont(..), Op(..))
import qualified Value as V
import StateT
import ContT
import Parse (parseList)
import Parser (Result(..), runParser)
import Error


-------------------------      pattern synonyms    -------------------

pattern PatForm1 :: String -> ScmVal -> ScmVal
pattern PatForm1 name arg1 = VCons (VSym name) (VCons arg1 VNil)

pattern PatForm2 :: String -> ScmVal -> ScmVal -> ScmVal
pattern PatForm2 name arg1 arg2 = VCons (VSym name) (VCons arg1 (VCons arg2 VNil))

pattern PatForm3 :: String -> ScmVal -> ScmVal -> ScmVal -> ScmVal
pattern PatForm3 name arg1 arg2 arg3 = VCons (VSym name) (VCons arg1 (VCons arg2 (VCons arg3 VNil)))


--------------------------         eval           ---------------------

throwE :: Exception e => e -> Eval a
throwE = liftIO . throwIO

runEval :: Eval ScmVal -> IO ScmVal
runEval ev = evalStateT (runContT ev return) [EvCont return]


eval :: Env -> ScmVal -> Eval ScmVal

eval _ VTrue = return VTrue
eval _ VFalse = return VFalse
eval _ (VChar c) = return $ VChar c
eval _ (VNum n) = return $ VNum n
eval _ (VStr s) = return $ VStr s

-- quote
eval _ (PatForm1 "quote" x) = return x

-- if
eval env (PatForm3 "if" test conseq alt) =
  do
    v <- eval env test
    if V.isTrue v
      then eval env conseq
      else eval env alt

--  one-armed if
eval env (PatForm2 "if" test conseq) =
  do
    v <- eval env test
    if V.isTrue v
      then eval env conseq
      else return VVoid

-- lambda
eval env (VCons (VSym "lambda")
                (VCons params
                       body))
  | not $ V.isParamList params = throwE $ InvalidSyntax $ "invalid lambda parameters: " ++ show params
  | not $ V.isList1 body = throwE $ InvalidSyntax $ "invalid lambda body: " ++ show body
  | otherwise = do
    ptr <- liftIO $ newIORef ()
    return $
      VClo { closurePtr = ptr
           , closureName = ""
           , closureParams = V.mkParams params
           , closureBody = V.toHsList body
           , closureEnv = env
           }

-- variable
eval env (VSym var) = do
  v <- liftIO $ applyEnv env var
  case v of
    Nothing -> throwE $ UnboundVariable var
    Just ref  -> do
      val <- liftIO $ readIORef ref
      if V.isMacro val
        then throwE $ InvalidSyntax $ "invalid macro use: " ++ var
        else return val

-- set!
eval env (PatForm2 "set!" (VSym var) exp) = do
  v <- liftIO $ applyEnv env var
  case v of
    Nothing -> throwE $ UnboundVariable var
    Just ref -> do
      val <- liftIO $ readIORef ref
      if V.isMacro val
        then throwE $ InvalidSyntax $ "invalid macro use: " ++ var
        else VVoid <$ (eval env exp >>= liftIO . updateEnv env var)

-- (define var exp)
eval env (PatForm2 "define" (VSym var) exp) =
  VVoid <$ (eval env exp >>= liftIO . updateEnv env var)

-- (define (var . args) exp1 exp ...)
eval env (VCons (VSym "define")
                (VCons (VCons (VSym name)
                              params)
                       body))
  | not $ V.isParamList params = throwE $ InvalidSyntax $ "invalid define parameters: " ++ show params
  | not $ V.isList1 body = throwE $ InvalidSyntax $ "invalid define body: " ++ show body
  | otherwise = eval env $
    VCons (VSym "define")
          (VCons (VSym name)
                 (VCons (VCons (VSym "lambda")
                               (VCons params body))
                        VNil))

-- (defmacro (var . args) exp1 exp ...)
eval env (VCons (VSym "defmacro")
                (VCons (VCons (VSym var)
                              params)
                       body))
  | not $ V.isParamList params = throwE $ InvalidSyntax $ "invalid defmacro parameters: " ++ show params
  | not $ V.isList1 body = throwE $ InvalidSyntax $ "invalid defmacro body: " ++ show body
  | otherwise =
      liftIO $ VVoid <$ updateEnv env var (VMacro
        {
          macroName = var
        , macroParams = V.mkParams params
        , macroBody = V.toHsList body
        , macroEnv = env
        })

-- (begin exp1 exp ...)
eval env (VCons (VSym "begin")
                body)
  | not $ V.isList1 body = throwE $ InvalidSyntax $ "invalid begin body: " ++ show body
  | otherwise = last <$> evalSeq env (V.toHsList body)

-- (quasiquote x)
eval env (PatForm1 "quasiquote" val) = do
  eval env $ expandQQ 0 val

-- function application
eval env xs@(VCons rator rands)
  | V.isSymbol rator = do -- may be macro use
    v <- liftIO $ applyEnv env $ V.symValue rator
    case v of
      Nothing -> app
      Just ref -> do
        val <- liftIO $ readIORef ref
        if V.isMacro val
          then applyMacro val (V.toHsList rands) >>= eval env
          else app
  | otherwise = app
  where
    app = do
      (rator':rands') <- evalSeq env $ V.toHsList xs
      apply env rator' rands'

-- invalid syntax
eval _ e = throwE $ InvalidSyntax $ "invalid syntax: " ++ show e

----------------------     auxiliary functions      --------------------

evalSeq :: Env -> [ScmVal] -> Eval [ScmVal]
evalSeq env exprs = mapM (eval env) exprs


loadFile :: Env -> String -> Eval ScmVal
loadFile env path = do
  text <- liftIO $ readFile path `catch`
            \(SomeException e) ->
              throwIO $ CustomError $ show e
  case runParser parseList text of
    Fail err -> throwE $ CustomError $ "parse error: " ++ err
    Succeed (exps, _) -> last <$> mapM (eval env) exps


apply :: Env -> ScmVal -> [ScmVal] -> Eval ScmVal
apply _ (VClo{ closureParams, closureBody, closureEnv }) args = do
  env <- liftIO $ storeArguments closureEnv closureParams args
  last <$> evalSeq env closureBody

apply _ (VPrim{primitiveFunc}) args = liftIO $ runPrimFunc primitiveFunc args

apply env (VOp OpApply) args = do
  unless (length args >= 2) $
    throwE $ ArityMismatch 2 (length args) True
  let fn    = head args
      args' = tail args
      rest  = last args'
  unless (V.isList rest) $
    throwE $ InvalidArgument $ "expected list, actual type: " ++ V.typeString rest
  apply env fn $ init args' ++ V.toHsList rest

apply env (VOp OpLoad) args = do
  unless (length args == 1) $
    throwE $ ArityMismatch 1 (length args) False
  let val = head args
  unless (V.isSameType (VStr "") val) $
    throwE $ InvalidArgument $ "expected type: string, actual type: " ++ V.typeString val
  loadFile env (strValue val)

apply _ v _ = throwE $ NonProcedure v


storeArguments :: Env -> Params -> [ScmVal] -> IO Env
storeArguments env params args = do
  val <- readIORef env
  env' <- newIORef val
  extendEnv params args env'
  return env'


applyMacro :: ScmVal -> [ScmVal] -> Eval ScmVal
applyMacro (VMacro{ macroParams, macroBody, macroEnv }) args = do
  env <- liftIO $ storeArguments macroEnv macroParams args
  last <$> evalSeq env macroBody
applyMacro _ _ = error "unreachable"


-------------------       environment functions     --------------

applyEnv :: Env -> String -> IO (Maybe (IORef ScmVal))
applyEnv env var = apply' <$> readIORef env
  where
    apply' :: [Frame] -> Maybe (IORef ScmVal)
    apply' [] = Nothing
    apply' (frm:env') = lookup var frm <|> apply' env'

--- update or insert
updateEnv :: Env -> String -> ScmVal -> IO ()
updateEnv env var val = do
  ref <- applyEnv env var
  case ref of
    Nothing -> do
      env' <- readIORef env
      ref' <- newIORef $ updateProcName val var
      writeIORef env $ ((var, ref') : head env') : tail env'
    Just ref' -> writeIORef ref' $ updateProcName val var

extendEnv :: Params -> [ScmVal] -> Env -> IO ()
extendEnv vars vals env = mkFrame vars vals >>= modifyIORef env . (:)
  where
    mkFrame :: Params -> [ScmVal] -> IO Frame
    mkFrame NoParams [] = return []
    mkFrame NoParams _  = throwIO $ ArityMismatch (V.paramsLength vars) (length vals) False
    mkFrame (VarParam var) vals' = do
      ref <- newIORef $ V.fromHsList vals'
      return [(var, ref)]
    mkFrame (Params var vars') (val:vals') = do
      ref <- newIORef $ updateProcName val var
      frm <- mkFrame vars' vals'
      return $ (var, ref) : frm
    mkFrame params [] = throwIO $ ArityMismatch (V.paramsLength vars) (length vals) (V.isVariadic params)

updateProcName :: ScmVal -> String -> ScmVal
updateProcName clo@(VClo { closureName="" }) name = clo { closureName=name }
updateProcName v _ = v


getVariables :: Env -> IO [String]
getVariables env = do
  frms <- readIORef env
  return $ nub $ concatMap (map fst) frms

----------------------       quasiquote         --------------------

pattern App1 :: String -> ScmVal -> ScmVal
pattern App1 f x = VCons (VSym f) (VCons x VNil)

pattern App2 :: String -> ScmVal -> ScmVal -> ScmVal
pattern App2 f x y = VCons (VSym f) (VCons x (VCons y VNil))

pattern Quote, Quasiquote, Unquote, UnquoteSplicing :: ScmVal -> ScmVal
pattern Quote x = App1 "quote" x
pattern Quasiquote x = App1 "quasiquote" x
pattern Unquote x = App1 "unquote" x
pattern UnquoteSplicing x = App1 "unquote-splicing" x


expandQQ :: Int -> ScmVal -> ScmVal

expandQQ n (Quasiquote x) =
  App2 "list" (Quote (VSym "quasiquote")) (expandQQ (n + 1) x)

expandQQ 0 (Unquote x) =
  x
expandQQ n (Unquote x) =
  App2 "list" (Quote (VSym "unquote")) (expandQQ (n - 1) x)

expandQQ 0 (VCons (UnquoteSplicing x) xs) =
  App2 "append" x (expandQQ 0 xs)
expandQQ n (VCons (UnquoteSplicing x) xs) =
  App2 "cons" (App2 "list" (Quote (VSym "unquote-splicing")) (expandQQ (n - 1) x)) (expandQQ n xs)

expandQQ n (VCons x xs) =
  App2 "cons" (expandQQ n x) (expandQQ n xs)

expandQQ _ x = Quote x

