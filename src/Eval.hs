{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Eval
  (
    eval
  , loadFile
  , getVariables
  , initialEnv
  , updateEnv
  ) where

import Value (ScmVal(..), ScmPrim(..), ScmError(..), Env, Frame)
import qualified Value as V
import Parse (parseList)
import Parser (Result(..), runParser)
import Control.Exception (throwIO, catch, SomeException(..))
import Control.Applicative ((<|>))
import Data.List (foldl', foldl1', nub)
import Data.IORef
import Prelude hiding (exp)


eval :: Env -> ScmVal -> IO ScmVal

eval _ VTrue = return VTrue
eval _ VFalse = return VFalse
eval _ (VChar c) = return $ VChar c
eval _ (VNum n) = return $ VNum n
eval _ (VStr s) = return $ VStr s

eval _ (VCons (VSym "quote")
              (VCons x VNil)) = return x

eval env (VCons (VSym "if")
                (VCons test
                       (VCons conseq
                              (VCons alt VNil)))) =
  do
    v <- eval env test
    if V.isTrue v
      then eval env conseq
      else eval env alt

--  one-armed if
eval env (VCons (VSym "if")
                (VCons test
                       (VCons conseq VNil))) =
  do
    v <- eval env test
    if V.isTrue v
      then eval env conseq
      else return VVoid

eval env (VCons (VSym "lambda")
                (VCons params
                       body))
  | not $ V.isParamList params = throwIO $ InvalidSyntax $ "invalid parameters: " ++ show params
  | not $ V.isList1 body = throwIO $ InvalidSyntax $ "invalid lambda body: " ++ show body
  | otherwise =
    return $
      VClo { closureName = ""
           , closureParams = params
           , closureBody = body
           , closureEnv = env
           }

eval env e@(VCons (VSym "let")
                  (VCons bindings
                         body))
  | not $ validBindings bindings = throwIO $ InvalidSyntax $ "invalid let syntax: " ++ show e
  | not $ V.isList1 body = throwIO $ InvalidSyntax $ "invalid let body: " ++ show body
  | otherwise =
    eval env (VCons (VCons (VSym "lambda")
                           (VCons (names bindings)
                                  body))
                    (values bindings))
  where
    validBindings :: ScmVal -> Bool
    validBindings VNil = True
    validBindings (VCons (VCons (VSym _)
                                (VCons _ VNil))
                         xs) = validBindings xs
    validBindings _ = False

    names :: ScmVal -> ScmVal
    names VNil = VNil
    names (VCons (VCons name
                        (VCons _ VNil))
                 xs) = VCons name $ names xs
    names _ = error "unreachable names"

    values :: ScmVal -> ScmVal
    values VNil = VNil
    values (VCons (VCons _ (VCons value VNil))
                 xs) = VCons value $ values xs
    values _ = error "unreachable values"


eval env (VSym var) = do
  v <- applyEnv env var
  case v of
    Nothing -> throwIO $ UnboundVariable var
    Just ref  -> readIORef ref

eval env (VCons (VSym "set!")
                (VCons (VSym var)
                       (VCons exp VNil))) = do
  v <- applyEnv env var
  case v of
    Nothing -> throwIO $ UnboundVariable var
    _ -> VVoid <$ (eval env exp >>= updateEnv env var)

eval env (VCons (VSym "define")
                (VCons (VSym var)
                       (VCons exp VNil))) =
  VVoid <$ (eval env exp >>= updateEnv env var)

eval env (VCons (VSym "define")
                (VCons (VCons (VSym var)
                              params)
                       body))
  | not $ V.isParamList params = throwIO $ InvalidSyntax $ "invalid parameters: " ++ show params
  | not $ V.isList1 body = throwIO $ InvalidSyntax $ "invalid body: " ++ show body
  | otherwise = eval env $
    (VCons (VSym "define")
           (VCons (VSym var)
                  (VCons (makeLambda params body)
                         VNil)))

eval env (VCons (VSym "begin")
                body)
  | not $ V.isList1 body = throwIO $ InvalidSyntax $ "invalid begin body: " ++ show body
  | otherwise = last <$> evalSeq env body

eval env (VCons (VSym "load")
                (VCons (VStr path)
                       VNil)) =
  loadFile env path

eval env e@(VCons _ rands)
  | not $ V.isList rands = throwIO $ InvalidSyntax $ "invalid function application: " ++ show e
  | otherwise = do
    (rator':rands') <- evalSeq env e
    apply rator' rands'

eval _ e = throwIO $ InvalidSyntax $ "invalid syntax: " ++ show e


evalSeq :: Env -> ScmVal -> IO [ScmVal]
evalSeq env exprs = mapM (eval env) $ V.toHsList exprs

makeLambda :: ScmVal -> ScmVal -> ScmVal
makeLambda params body =
  (VCons (VSym "lambda")
         (VCons params
                body))

loadFile :: Env -> String -> IO ScmVal
loadFile env path = do
  text <- readFile path `catch`
            \(SomeException e) ->
              throwIO $ CustomError $ show e
  case runParser parseList text of
    Fail err -> throwIO $ CustomError $ "parse error: " ++ err
    Succeed (exps, _) -> last <$> mapM (eval env) exps



apply :: ScmVal -> [ScmVal] -> IO ScmVal
apply (VClo{closureParams, closureBody, closureEnv}) args = do
  env0 <- readIORef closureEnv
  env <- newIORef env0
  extendEnv closureParams args env
  last <$> evalSeq env closureBody

apply (VPrim{primitiveFunc}) args = runPrimFunc primitiveFunc args

apply v _ = throwIO $ NonProcedure v


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

extendEnv :: ScmVal -> [ScmVal] -> Env -> IO ()
extendEnv vars vals env = makeFrame vars vals >>= modifyIORef env . (:)
  where
    makeFrame :: ScmVal -> [ScmVal] -> IO Frame
    makeFrame VNil [] = return []
    makeFrame VNil _    = throwIO $ ArityMismatch (V.listLength vars) (length vals) False
    makeFrame (VSym var') val' = do
      ref <- newIORef $ V.fromHsList val'
      return [(var', ref)]
    makeFrame _ [] = throwIO $ ArityMismatch (V.listLength' vars) (length vals) True
    makeFrame (VCons (VSym var') vars') (val':vals') = do
      frm <- makeFrame vars' vals'
      val <- newIORef $ updateProcName val' var'
      return $ (var', val) : frm
    makeFrame _ _ = error "unreachable makeFrame"


updateProcName :: ScmVal -> String -> ScmVal
updateProcName clo@(VClo { closureName="" }) name = clo { closureName=name }
updateProcName v _ = v


getVariables :: Env -> IO [String]
getVariables env = do
  frms <- readIORef env
  return $ nub $ concatMap (map fst) frms

initialEnv :: IO Env
initialEnv = do
  frm <- mapM (\(var, val) -> (,) var <$> newIORef val) initialBindings
  newIORef [frm]

initialBindings :: [(String, ScmVal)]
initialBindings =
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
          return $ V.fromBool $ V.isSameType VNil (head args))
  , ("+", VPrim "+" $
      ScmPrim $ \args ->
        assertAllArgTypes (VNum 1) args $
          return $ VNum $ foldl' (+) 0 $ map numValue args)
  , ("-", VPrim "-" $
      ScmPrim $ \args ->
        assertMoreArgc 1 args $
          assertAllArgTypes (VNum 1) args $
            if length args == 1
              then return $ VNum $ negate $ numValue $ head args
              else return $ VNum $ foldl1' (-) $ map numValue args)
  , ("*", VPrim "*" $
      ScmPrim $ \args ->
        assertAllArgTypes (VNum 1) args $
          return $ VNum $ foldl' (*) 1 $ map numValue args)
  , ("/", VPrim "/" $
      ScmPrim $ \args ->
        assertMoreArgc 1 args $
          assertAllArgTypes (VNum 1) args $
            if length args == 1
              then return $ VNum $ recip $ numValue $ head args
              else return $ VNum $ foldl1' (/) $ map numValue args)
  , (">", VPrim ">" $
      ScmPrim $ \args ->
        assertArgc 2 args $
          assertAllArgTypes (VNum 1) args $
            let (VNum a) = head args
                (VNum b) = args !! 1
            in return $ V.fromBool $ a > b)
  , (">=", VPrim ">=" $
      ScmPrim $ \args ->
        assertArgc 2 args $
          assertAllArgTypes (VNum 1) args $
            let (VNum a) = head args
                (VNum b) = args !! 1
            in return $ V.fromBool $ a >= b)
  , ("<", VPrim "<" $
      ScmPrim $ \args ->
        assertArgc 2 args $
          assertAllArgTypes (VNum 1) args $
            let (VNum a) = head args
                (VNum b) = args !! 1
            in return $ V.fromBool $ a < b)
  , ("<=", VPrim "<=" $
      ScmPrim $ \args ->
        assertArgc 2 args $
          assertAllArgTypes (VNum 1) args $
            let (VNum a) = head args
                (VNum b) = args !! 1
            in return $ V.fromBool $ a <= b)
  , ("=", VPrim "=" $
      ScmPrim $ \args ->
        assertArgc 2 args $
          assertAllArgTypes (VNum 1) args $
            let (VNum a) = head args
                (VNum b) = args !! 1
            in return $ V.fromBool $ a == b)
  , ("print", VPrim "print" $
      ScmPrim $ \args ->
        assertArgc 1 args $
          VVoid <$ (print $ head args))
  , ("apply", VPrim "apply" $
      ScmPrim $ \args ->
        assertMoreArgc 2 args $
          let fn = head args
              args' = tail args
          in if V.isList $ last args'
               then apply fn $ init args' ++ V.toHsList (last args')
               else throwIO $ InvalidArgument $ "expected list, actual type: " ++ V.typeString (last args'))
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
  if V.isSameType expected actual
    then val
    else throwIO $ InvalidArgument $ "expected type: " ++ V.typeString expected ++ ", actual type: " ++ V.typeString actual

assertAllArgTypes :: ScmVal -> [ScmVal] -> IO ScmVal -> IO ScmVal
assertAllArgTypes expected actuals ~val =
  if all (V.isSameType expected) actuals
    then val
    else throwIO $ InvalidArgument $ "expected type: " ++ V.typeString expected