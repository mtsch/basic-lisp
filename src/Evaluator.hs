module Evaluator
where

import           Data.HashMap.Lazy (HashMap, (!))
import qualified Data.HashMap.Lazy as Map
import           Data.Maybe
import           Data.Vector.Persistent (Vector)
import qualified Data.Vector.Persistent as Vector

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
      (Atom a)  -> (env, find env a)
      (Vec vec) -> (env, Vec . fmap (snd . eval env) $ vec)
      (List l)  -> evalList env l
      sexpr     -> (env, sexpr)

-- "Call" a list. If the first argument is an atom, it's treated as a function,
-- if it's a vector, it's indexed.
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

      -- Vector indexing.
      [Vec vec, Integer i] ->
          (env, fromMaybe Nil (Vector.index vec i))

      -- Variable definition.
      [Atom "def", Atom name, sexpr] ->
          let (_, rhs) = eval env sexpr
              env'     = Map.insert name rhs env
          in
            (env', rhs)

      -- if-then-else
      [Atom "if", pred, th, el] ->
          case eval env pred of
            (env', Bool False) -> eval env el
            (env', Nil)        -> eval env el
            (env', _)          -> eval env th

      -- Function definition
      [Atom "fn", List args, List body] ->
          (env, if not . all isAtom $ args
                then Err "Bad function declaration!"
                else Fun (map toString args) env (List body))
            where
              toString (Atom s) = s

      -- Primitive
      Primitive p : args ->
          (env, p . map (snd . eval env) $ args)

      -- Function call
      fun@Fun {} : args ->
          callFun env fun args

      -- Atom that needs to be resolved
      Atom a : rest ->
          let (_, resolved) = eval env (Atom a)
          in
            if resolved == Nil
            then (env, Err $ "Unknown function \"" ++ a ++ "\"!")
            else evalList env (resolved : rest)

      -- Expression that needs to be evaluated
      exp@(List _) : rest ->
          let (env', resolved) = eval env exp
          in
            evalList env' (resolved : rest)

-- Find variable in environment.
find :: Environment -> String -> SExpr
find env name = fromMaybe Nil (Map.lookup name env)

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
