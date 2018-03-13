module Eval where

import Parse
import StateT
import Value (ScmVal(..), ScmError(..), Env, Store)
import qualified Value as Scm
import Control.Exception (throwIO)
import Control.Monad.State.Class (get, put)
import Control.Applicative ((<|>))


type St = (Env, Store)

type EvalT = StateT St IO


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
                              (VCons alt VNil))) =
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
      else return VFalse

evalExpr (VCons (VSym "lambda")
                (VCons params
                       body))
  | not $ Scm.isParamList params = liftIO $ throwIO $ InvalidSyntax $ "invalid parameters: " ++ show params
  | not $ Scm.isList1 body = liftIO $ throwIO $ InvalidSyntax $ "invalid body: " ++ show body
  | otherwise = do
    (env, _) <- get
    return $
      VClo { closureName = "",
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



applyEnv :: St -> String -> Maybe ScmVal
applyEnv (env, store) var = apply env var
  where
    appply :: Env -> String -> Maybe ScmVal
    apply [] _ = Nothing
    apply (i:is) s = Scm.searchFrame s (Scm.getFrame store i) <|> apply is s

--- update or insert
updateEnv :: St -> String -> ScmVal -> St
updateEnv (env@(i:_), store) var val =
  (env, Scm.updateStore store i $
          Scm.updateFrame var (updateProcName val var) $
            Scm.getFrame store i)

extendEnv :: ScmVal -> ScmVal -> St -> IO St
extendEnv vars vals (env, store) = do
  bindings <- makeBindings vars vals
  return (i:env, Scm.updateStore store' i $
                   Scm.makeFrame bindings)
  where
    (i, store') = Scm.extendStore store

    -- update proc name
    makeBindings :: ScmVal -> ScmVal -> IO [(String, ScmVal)]
    makeBindings VNil VNil = return []
    makeBindings VNil _    = throwIO $ ArityMismatch (Scm.listLength vars) (Scm.listLength vals) False
    makeBindings (VSym var') val' = return [(var', val')]
    makeBindings _ VNil = throwIO $ ArityMismatch (Scm.listLength' vars) (Scm.listLength vals) True
    makeBindings (VCons var' vars') (VCons val' vals') = ((var', val') :) <$> makeBindings vars' vals'


updateProcName :: ScmVal -> String -> ScmVal
updateProcName clo@(VClo { closureName="" }) name =
  clo { closureName=name }
updateProcName v _ = v