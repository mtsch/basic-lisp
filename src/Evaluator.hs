module Evaluator
where

import           Data.HashMap.Lazy (HashMap, (!))
import qualified Data.HashMap.Lazy as Map
import           Data.Maybe

import           SExpr
import           Reader

-- Evaluate a multiple S-Expressions in a sequential fashion.
-- Return the last result in list.
evalMulti :: Environment -> [SExpr] -> EnvSExpr
evalMulti env []       = (env, Nil)
evalMulti env [ex]     = eval env ex
evalMulti env (ex:exs) = let (env', _) = eval env ex
                         in
                           evalMulti env' exs

-- Evaluate an S-Expression.
eval :: Environment -> SExpr -> EnvSExpr
eval env sexpr =
    case sexpr of
      (Atom a)  -> (env, resolveName env a)
      (List l)  -> evalList env l
      sexpr     -> (env, sexpr)

-- Evaluate a list. This can be a function call, vector indexing, or a special
-- form such as if-then-else.
evalList :: Environment -> [SExpr] -> EnvSExpr
evalList env expressions =
    case expressions of

      -- Can't call an integer, a string, a bool or nil
      Integer _ : _ ->
          (env, Err "Can't call an integer!")
      String _ : _ ->
          (env, Err "Can't call a string!")
      Bool _ : _ ->
          (env, Err "Can't call a bool!")
      Nil : _ ->
          (env, Err "Can't call nil!")

      -- Quoted list.
      [Atom "quote", l] -> (env, l)

      -- Variable definition.
      [Atom "def", Atom name, sexpr] ->
          let (_, rhs) = eval env sexpr
              env'     = Map.insert name rhs env
          in
            (env', rhs)

      -- if-then-else.
      [Atom "if", pred, th, el] ->
          case eval env pred of
            (env', Bool False) -> eval env el
            (env', List [])    -> eval env el
            (env', Nil)        -> eval env el
            (env', _)          -> eval env th

      -- Function definition.
      [Atom "fn", List args, List body] ->
          (env, if not . all isAtom $ args
                then Err "Bad function declaration!"
                else Fun (map toString args) env (List body))
            where
              toString (Atom s) = s

      -- Primitive function call.
      Primitive p : args ->
          (env, p . map (snd . eval env) $ args)

      -- Function call.
      fun@Fun {} : args ->
          callFun env fun args

      -- Atom that needs to be resolved.
      Atom a : rest ->
          let (_, resolved) = eval env (Atom a)
          in
            if resolved == Nil
            then (env, Err $ "Unknown function \"" ++ a ++ "\"!")
            else evalList env (resolved : rest)

      -- Expression that needs to be evaluated.
      exp@(List _) : rest ->
          let (env', resolved) = eval env exp
          in
            evalList env' (resolved : rest)

-- Call a function.
callFun :: Environment -> SExpr -> [SExpr] -> EnvSExpr
callFun env fun args
    | not . isFun $ fun           = error "Trying to call a non-function!"
    | length args /= length names = (env, Err "Wrong airity!")
    | otherwise =
        let evaldArgs = map (snd . eval env) args
            argEnv    = Map.fromList $ [("recur", fun)] ++ zip names evaldArgs
        in
          eval (Map.unions [argEnv, closure fun, env]) $ body fun
    where
      names = argNames fun
