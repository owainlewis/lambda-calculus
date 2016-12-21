module Language.Interpreter
  (  Term(..)
   , Env(..) 
  ) where

import           Control.Monad.Reader
import           Data.Monoid          ((<>))

data Term = Apply Term Term 
          | Lambda String Term 
          | Var String deriving (Show)

newtype Env = Env ([(String,Closure)])

type Closure = (Term,Env)

data Value = Lam String Closure 
           | Failure String

instance Show Value where
  show (Lam x (term, _)) = x <> show term
  show (Failure f) = show f

interp' :: Term -> Reader Env Value
-- when we have lambda term, we can just return it
interp' (Lambda nv t)
   = do env <- ask
        return $ Lam nv (t,env)
-- when we run into a value we look it up in the environment
interp' (Var v)
   = do (Env env) <- ask
        case lookup (show v) env of
          -- if it is not in the environment we have a problem
          Nothing -> return . Failure $ "unbound variable: " ++ (show v)
          -- if it is in the environment, than we should interpret it
          Just (term,env) -> local (const env) $ interp' term
interp' (Apply t1 t2)
   = do v1 <- interp' t1
        case v1 of
           Failure s -> return (Failure s)
           Lam nv clos -> local (\(Env ls) -> Env ((nv,clos):ls)) $ interp' t2

interp :: Term -> Value
interp term = runReader (interp' term) (Env [])
