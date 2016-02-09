{-# OPTIONS_GHC -Wall #-}
module Evaluator
where

import qualified Data.HashMap.Lazy as Map

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
eval env expression =
    case expression of
      (Atom a) -> return (env, resolveName env a)
      (List l) -> evalListBody env l
      value    -> return (env, value)

-- Evaluate the body of a list.
-- This can be a function call or a special form such as def or if-then-else.
evalListBody :: Environment -> [SExpr] -> IO EnvSExpr
evalListBody env expressions =
    case expressions of

      -- Variable definition.
      [Atom "def", Atom name, sexpr] ->
          do (_, rhs) <- eval env sexpr
             let env' = Map.insert name rhs env
             return (env', rhs)

      -- if-then-else.
      [Atom "if", predicate, th, el] ->
          do evaldPred <- eval env predicate
             case evaldPred of
               (env', Bool False) -> eval env' el
               (env', List [])    -> eval env' el
               (env', Nil)        -> eval env' el
               (env', _)          -> eval env' th

      -- Function declaration.
      [Atom "fun", List args, List expr] ->
          return (env, if not . all isAtom $ args
                       then Err "Bad function declaration!"
                       else Fun (map unpackAtom args) env (List expr))

      -- Bad function declaration.
      Atom "fun" : _ ->
          return (env, Err "Bad function declaration!")

      -- Eval special form
      Atom "eval" : args ->
          evalSpecialForm env args

      -- Quoted lists.
      Atom "quote" : [list@(List _)] ->
          return (env, list)

      -- Bad quote.
      Atom "quote" : _ ->
          return (env, Err "This is not how you use quote!")

      -- Primitive function call.
      Primitive p : args ->
          do result <- p =<< mapM (fmap snd . eval env) args
             return (env, result)

      -- Function call.
      fun@Fun {} : args ->
          callFunction env fun args

      -- Atom that needs to be resolved.
      Atom a : rest ->
          do resolved <- snd <$> eval env (Atom a)
             evalListBody env (resolved : rest)

      -- Expression that needs to be evaluated.
      expr@(List _) : rest ->
          do (env', resolved) <- eval env expr
             evalListBody env' (resolved : rest)

      -- Errors propagate.
      err@(Err _) : _ ->
          return (env, err)

      -- Can't call an integer, a string, a bool or nil
      (Int _) : _ ->
          return (env, Err "Can't call an int!")

      (String _) : _ ->
          return (env, Err "Can't call a string!")

      (Bool _) : _ ->
          return (env, Err "Can't call a bool!")

      Nil : _ ->
          return (env, Err "Can't call nil!")

      -- Empty list doesn't evaluate.
      [] ->
          return (env, List [])

-- Call a function.
callFunction :: Environment -> SExpr -> [SExpr] -> IO EnvSExpr
callFunction env fun args
    | not $ isFun fun             = error "Trying to call a non-function!"
    | length args /= length names = return (env, Err "Wrong airity!")
    | otherwise =
        do evaldArgs <- mapM (fmap snd . eval env) args
           let argEnv = Map.fromList $ ("recur", fun) : zip names evaldArgs
           eval (Map.unions [argEnv, closure fun, env]) $ body fun
    where
      names = argNames fun

-- The eval command can read and evaluate strings and evaluate (quoted) lists.
evalSpecialForm :: Environment -> [SExpr] -> IO EnvSExpr
evalSpecialForm env expressions =
    case expressions of

      -- Evaluate string(s).
      args | all isString args ->
          evalMulti env $ concatMap (readSExpr . unpackString) args

      -- Evaluate quoted list.
      [List [Atom "quote", List l]] ->
          evalListBody env l

      -- Evaluate list.
      [List l] ->
          evalListBody env l

      -- Eval atom else.
      [atom@(Atom _)] ->
           do result <- snd <$> eval env atom
              evalListBody env [Atom "eval", result]

      -- Bad eval.
      _ ->
          return (env, Err "Can't eval that!")
