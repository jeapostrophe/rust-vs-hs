#!/usr/bin/env stack
{- stack script
 --optimize
 --ghc-options -Wall
 --resolver    lts-17.8
 -}
{-# LANGUAGE RecordWildCards, LambdaCase #-}

import Control.Monad.Reader
import Control.Monad.Except
import qualified Data.Map.Strict as M

-- Language
data Prim
  = PAdd
  | PMul
  | PDiv
  | PNumToStr
  deriving (Show)

data Val
  = VNum Int
  | VStr String
  deriving (Show)

data Expr
  = EVar String
  | EVal Val
  | EApp Prim [Expr]
  | ELet String Expr Expr
  | EDbg Expr
  deriving (Show)

-- No Debug
class NoDebug a where
  noDbg :: a -> a

instance NoDebug Expr where
  noDbg = \case
    EVar x -> EVar x
    EVal x -> EVal x
    EApp p es -> EApp p (noDbg es)
    ELet x xe e -> ELet x (noDbg xe) (noDbg e)
    EDbg e -> noDbg e

instance NoDebug a => NoDebug [a] where
  noDbg = map noDbg

-- All Debug
class AllDebug a where
  allDbg :: a -> a

instance AllDebug Expr where
  allDbg = \case
    EVar x -> EVar x
    EVal x -> EVal x
    EApp p es -> EDbg $ EApp p (allDbg es)
    ELet x xe e -> ELet x (allDbg xe) (allDbg e)
    EDbg e -> allDbg e

instance AllDebug a => AllDebug [a] where
  allDbg = map allDbg

-- Evaluator
data EvalEnv = EvalEnv
  { eeEnv :: M.Map String Val
  }

type EvalApp = ExceptT String (ReaderT EvalEnv IO)

evalPrim :: (Prim, [Val]) -> EvalApp Val
evalPrim = \case
  (PAdd, [ VNum x, VNum y ]) ->
    return $ VNum $ x + y
  (PMul, [ VNum x, VNum y ]) ->
    return $ VNum $ x * y
  (PDiv, [ VNum x, VNum y ]) | y /= 0 ->
    return $ VNum $ x `div` y
  (PNumToStr, [ VNum x ]) ->
    return $ VStr $ show x
  _ -> throwError "bad prim"

class Eval a where
  eval :: a -> EvalApp Val

instance Eval Val where
  eval = return

instance Eval Expr where
  eval = \case
    EVar v -> do
      env <- asks eeEnv
      case M.lookup v env of
        Just x -> return x
        Nothing -> throwError $ "unbound var: " <> show v
    EVal v -> eval v
    EApp p es -> do
      vs <- mapM eval es
      evalPrim (p, vs)
    ELet x xe ne -> do
      xe' <- eval xe
      local (\e -> e { eeEnv = M.insert x xe' $ eeEnv e }) $
        eval ne
    EDbg e -> do
      liftIO $ putStrLn $ "- " <> show e
      e' <- eval e
      liftIO $ putStrLn $ "+ " <> show e'
      return e'

eval_ :: Eval a => a -> IO ()
eval_ x = do
  let eeEnv = mempty
  a <-
    flip runReaderT (EvalEnv {..}) $
      runExceptT $ eval x
  putStrLn $ show a

-- Examples
e1 = id
  $ ELet "x" (EVal $ VNum 5)
  $ ELet "y" (EVal $ VNum 4)
  $ EDbg
  $ EApp PAdd
    [ EVar "x"
    , EApp PMul
      [ EVar "x"
      , EVar "y"
      ]
    ]

e2 = allDbg
  $ EApp PAdd
    [ EApp PNumToStr
      [ EVal $ VNum 5 ]
    , EVal $ VNum 5
    ]

e3 = id
  $ EApp PDiv
    [ EVal $ VNum 5
    , EVal $ VNum 0
    ]

main :: IO ()
main = do
  eval_ e1
  eval_ e2
  eval_ e3
