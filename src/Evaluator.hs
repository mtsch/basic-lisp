module Evaluator
where

import           Data.HashMap.Lazy (HashMap, (!))
import qualified Data.HashMap.Lazy as Map
import           Data.Maybe

import           SExpr
import           Reader

-- Evaluate a multiple S-Expressions in a sequential fashion.
-- Return the last result in list.
evalMulti :: Environment -> [SExpr] -> IO EnvSExpr
evalMulti env []       = return (env, Nil)
evalMulti env [ex]     = eval env ex
evalMulti env (ex:exs) = do (env', _) <- eval env ex
                            evalMulti env' exs

-- Evaluate an S-Expression.
eval :: Environment -> SExpr -> IO EnvSExpr
eval env sexpr =
    case sexpr of
      (Atom a)  -> return (env, resolveName env a)
      (List l)  -> evalList env l
      sexpr     -> return (env, sexpr)

-- Evaluate a list. This can be a function call or a special form such as def.
evalList :: Environment -> [SExpr] -> IO EnvSExpr
evalList env expressions =
    case expressions of

      -- Can't call an integer, a string, a bool or nil
      Integer _ : _ ->
          return (env, Err "Can't call an integer!")
      String _ : _ ->
          return (env, Err "Can't call a string!")
      Bool _ : _ ->
          return (env, Err "Can't call a bool!")
      Nil : _ ->
          return (env, Err "Can't call nil!")

      -- Variable definition.
      [Atom "def", Atom name, sexpr] ->
          do (_, rhs) <- eval env sexpr
             let env' = Map.insert name rhs env
             return (env', rhs)

      -- if-then-else.
      [Atom "if", pred, th, el] ->
          do evaldPred <- eval env pred
             case evaldPred of
               (env', Bool False) -> eval env el
               (env', List [])    -> eval env el
               (env', Nil)        -> eval env el
               (env', _)          -> eval env th

      -- Function definition.
      [Atom "fun", List args, List body] ->
          return (env, if not . all isAtom $ args
                       then Err "Bad function declaration!"
                       else Fun (map toString args) env (List body))
            where
              toString (Atom s) = s

      -- Evaluate string(s).
      Atom "eval" : args
          | all isString args ->
          evalMulti env $ concatMap (readSExpr . unpackString) args

      -- Evaluate quoted list.
      Atom "eval" : [List [Atom "quote", List l]] ->
          evalList env l

      -- Bad eval.
      Atom "eval" : _ ->
          return (env, Err "Can't eval that!")

      -- Primitive function call.
      Primitive p : args ->
          do result <- p =<< mapM (fmap snd . eval env) args
             return (env, result)

      -- Function call.
      fun@Fun {} : args ->
          callFun env fun args

      -- Atom that needs to be resolved.
      Atom a : rest ->
          do (_, resolved) <- eval env (Atom a)
             if resolved == Nil
               then return (env, Err $ "Unknown function \"" ++ a ++ "\"!")
               else evalList env (resolved : rest)

      -- Errors propagate.
      err@(Err _) : _ ->
          return (env, err)

      -- Expression that needs to be evaluated.
      exp@(List _) : rest ->
          do (env', resolved) <- eval env exp
             evalList env' (resolved : rest)

-- Call a function.
callFun :: Environment -> SExpr -> [SExpr] -> IO EnvSExpr
callFun env fun args
    | not $ isFun fun             = error "Trying to call a non-function!"
    | length args /= length names = return (env, Err "Wrong airity!")
    | otherwise =
        do evaldArgs <- mapM (fmap snd . eval env) args
           let argEnv = Map.fromList $ ("recur", fun) : zip names evaldArgs
           eval (Map.unions [argEnv, closure fun, env]) $ body fun
    where
      names = argNames fun
