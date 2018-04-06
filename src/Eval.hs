{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Eval
  (
    eval
  , loadFile
  , apply
  , getVariables
  , newPtr
  , throwE
  , Eval
  ) where

import Prelude hiding (exp)
import Control.Exception (throwIO, catch, Exception, SomeException(..))
import Control.Applicative ((<|>))
import Control.Monad.IO.Class
import Control.Monad (zipWithM)
import Data.List (nub)
import Data.IORef
import Value (ScmVal(..), ScmPrim(..), Env, Frame, FakePtr)
import qualified Value as V
import Program (ScmProg(..))
import qualified Program as P
import Parse (parseList)
import Parser (Result(..), runParser)
import StateT
import Error


--------------------------         eval           ---------------------

type Eval = StateT FakePtr IO


throwE :: Exception e => e -> Eval a
throwE = liftIO . throwIO


eval :: Env -> ScmProg -> Eval ScmVal

eval _ PTrue = return VTrue
eval _ PFalse = return VFalse
eval _ (PChar c) = return $ VChar c
eval _ (PNum n) = return $ VNum n
eval _ (PStr s) = (`VStr` s) <$> newPtr

-- quote
eval _ (PList [PSym "quote", x]) = progToVal x

-- if
eval env (PList [PSym "if", test, conseq, alt]) =
  do
    v <- eval env test
    if V.isTrue v
      then eval env conseq
      else eval env alt

--  one-armed if
eval env (PList [PSym "if", test, conseq]) =
  do
    v <- eval env test
    if V.isTrue v
      then eval env conseq
      else return VVoid

--  cond
eval env (PList (PSym "cond" : body))
  | null body = throwE $ InvalidSyntax $ "empty cond body: " ++ show body
  | not $ all isClause body = throwE $ InvalidSyntax $ "invalid cond body: " ++ show body
  | otherwise = loop body
  where
    isClause (PList [_, PSym "=>", _]) = True
    isClause (PList (_:_)) = True
    isClause _ = False

    loop :: [ScmProg] -> Eval ScmVal
    loop [] = return VVoid
    loop [PList (PSym "else" : conseq)] = last <$> evalSeq env conseq
    loop (PList [test, PSym "=>", exp] : xs) = do
      v <- eval env test
      if V.isTrue v
        then eval env exp >>= (`apply` [v])
        else loop xs
    loop (PList (test:conseq):xs) = do
      v <- eval env test
      if V.isTrue v
        then if null conseq
               then return v
               else last <$> evalSeq env conseq
        else loop xs
    loop _ = error "unreachable"

-- lambda
eval env (PList (PSym "lambda":params:body))
  | not $ P.isParamList params = throwE $ InvalidSyntax $ "invalid lambda parameters: " ++ show params
  | null body = throwE $ InvalidSyntax $ "invalid lambda body: " ++ show body
  | otherwise = do
    ptr <- newPtr
    return $
      VClo { closurePtr = ptr
           , closureName = ""
           , closureParams = P.getParamList params
           , closureVararg = P.getParamVar params
           , closureBody = body
           , closureEnv = env
           }

-- variable
eval env (PSym var) = liftIO $ do
  v <- applyEnv env var
  case v of
    Nothing -> throwIO $ UnboundVariable var
    Just ref  -> do
      val <- readIORef ref
      if V.isMacro val
        then throwIO $ InvalidSyntax $ "invalid macro use: " ++ var
        else return val

-- set!
eval env (PList [PSym "set!", PSym var, exp]) = do
  v <- liftIO $ applyEnv env var
  case v of
    Nothing -> throwE $ UnboundVariable var
    Just ref -> do
      val <- liftIO $ readIORef ref
      if V.isMacro val
        then throwE $ InvalidSyntax $ "invalid macro use: " ++ var
        else VVoid <$ (eval env exp >>= liftIO . updateEnv env var)

-- (define var exp)
eval env (PList [PSym "define", PSym var, exp]) =
  VVoid <$ (eval env exp >>= liftIO . updateEnv env var)

-- (define (var . args) exp1 exp ...)
eval env (PList (PSym "define":header:body))
  | not $ P.isDefineHeader header = throwE $ InvalidSyntax $ "invalid define header: " ++ show header
  | null body = throwE $ InvalidSyntax $ "empty define body: " ++ show body
  | otherwise = eval env $
    PList [PSym "define", P.getDefineName header, PList (PSym "lambda" : P.getDefineParams header : body)]

-- (defmacro (var . args) exp1 exp ...)
eval env (PList (PSym "defmacro":header:body))
  | not $ P.isDefineHeader header = throwE $ InvalidSyntax $ "invalid defmacro header: " ++ show header
  | null body = throwE $ InvalidSyntax $ "empty defmacro body: " ++ show body
  | otherwise =
    let (PSym var) = P.getDefineName header
        params = P.getDefineParams header
    in
      liftIO $ VVoid <$ updateEnv env var (VMacro
        {
          macroName = var
        , macroParams = P.getParamList params
        , macroVararg = P.getParamVar params
        , macroBody = body
        , macroEnv = env
        })

-- (begin exp1 exp ...)
eval env (PList (PSym "begin":body))
  | null body = throwE $ InvalidSyntax $ "invalid begin body: " ++ show body
  | otherwise = last <$> evalSeq env body

-- (load "path/to/source/file")
eval env (PList [PSym "load", PStr path]) =
  loadFile env path

-- function application
eval env (PList xs@(rator:rands))
  | P.isSymbol rator = do -- may be macro use
    v <- liftIO $ applyEnv env $ P.symProg rator
    case v of
      Nothing -> app
      Just ref -> do
        val <- liftIO $ readIORef ref
        if V.isMacro val
          then applyMacro val rands >>= liftIO . valToProg >>= eval env
          else app
  | otherwise = app
  where
    app = do
      (rator':rands') <- evalSeq env xs
      apply rator' rands'

-- invalid syntax
eval _ e = throwE $ InvalidSyntax $ "invalid syntax: " ++ show e

----------------------     auxiliary functions      --------------------

progToVal :: ScmProg -> Eval ScmVal
progToVal (PChar c) = return $ VChar c
progToVal PTrue = return VTrue
progToVal PFalse = return VFalse
progToVal (PNum n) = return $ VNum n
progToVal (PSym s) = return $ VSym s
progToVal (PStr s) = (`VStr` s) <$> newPtr
progToVal (PList []) = return VNil
progToVal (PList (x:xs)) = VCons <$> newPtr <*> progToVal x <*> progToVal (PList xs)
progToVal (PDList [] y) = progToVal y
progToVal (PDList (x:xs) y) = VCons <$> newPtr <*> progToVal x <*> progToVal (PDList xs y)

valToProg :: ScmVal -> IO ScmProg
valToProg VNil = return $ PList []
valToProg (VChar c) = return $ PChar c
valToProg VTrue = return PTrue
valToProg VFalse = return PFalse
valToProg (VNum n) = return $ PNum n
valToProg (VSym s) = return $ PSym s
valToProg (VStr _ s) = return $ PStr s
valToProg (VCons _ x xs) = do
  x'  <- valToProg x
  xs' <- valToProg xs
  case xs' of
    PList xs''    -> return $ PList $ x' : xs''
    PDList xs'' y -> return $ PDList (x' : xs'') y
    _             -> return $ PDList [x'] xs'
valToProg x = throwIO $ InvalidSyntax $ "invalid syntax: " ++ show x

newPtr :: Monad m => StateT FakePtr m FakePtr
newPtr = StateT $ \n -> return (n, n + 1)


evalSeq :: Env -> [ScmProg] -> Eval [ScmVal]
evalSeq env exprs = mapM (eval env) exprs


loadFile :: Env -> String -> Eval ScmVal
loadFile env path = do
  text <- liftIO $ readFile path `catch`
            \(SomeException e) ->
              throwIO $ CustomError $ show e
  case runParser parseList text of
    Fail err -> throwE $ CustomError $ "parse error: " ++ err
    Succeed (exps, _) -> last <$> mapM (eval env) exps


apply :: ScmVal -> [ScmVal] -> Eval ScmVal
apply (VClo{ closureParams, closureVararg, closureBody, closureEnv }) args = do
  env0 <- liftIO $ readIORef closureEnv
  env <- liftIO $ newIORef env0
  extendEnv closureParams closureVararg args env
  last <$> evalSeq env closureBody

apply (VPrim{primitiveFunc}) args = runPrimFunc primitiveFunc args

apply v _ = throwE $ NonProcedure v


applyMacro :: ScmVal -> [ScmProg] -> Eval ScmVal
applyMacro (VMacro{ macroParams, macroVararg, macroBody, macroEnv }) args = do
  ptr <- newPtr
  args' <- mapM progToVal args
  apply (VClo { closurePtr = ptr
              , closureName = ""
              , closureParams = macroParams
              , closureVararg = macroVararg
              , closureBody = macroBody
              , closureEnv = macroEnv
              }) args'
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
    Just ref' -> writeIORef ref' val

extendEnv :: [String] -> Maybe String -> [ScmVal] -> Env -> Eval ()
extendEnv vars Nothing vals env
  | length vars /= length vals = throwE $ ArityMismatch (length vars) (length vals) False
  | otherwise = liftIO $ mkFrame vars vals >>= modifyIORef env . (:)

extendEnv vars (Just vararg) vals env
  | length vars > length vals = throwE $ ArityMismatch (length vars) (length vals) True
  | otherwise = do
    let (vals', restVals) = splitAt (length vars) vals
    frm <- liftIO $ mkFrame vars vals'
    restVal <- listToVal restVals
    ref <- liftIO $ newIORef $ updateProcName restVal vararg
    liftIO $ modifyIORef env (((vararg, ref) : frm) :)
  where
    listToVal :: [ScmVal] -> Eval ScmVal
    listToVal [] = return VNil
    listToVal (x:xs) = (`VCons` x) <$> newPtr <*> listToVal xs

mkFrame :: [String] -> [ScmVal] -> IO Frame
mkFrame vars vals = zipWithM mkBinding vars vals
  where
    mkBinding :: String -> ScmVal -> IO (String, IORef ScmVal)
    mkBinding var val = do
      ref <- newIORef $ updateProcName val var
      return (var, ref)

updateProcName :: ScmVal -> String -> ScmVal
updateProcName clo@(VClo { closureName="" }) name = clo { closureName=name }
updateProcName v _ = v


getVariables :: Env -> IO [String]
getVariables env = do
  frms <- readIORef env
  return $ nub $ concatMap (map fst) frms
