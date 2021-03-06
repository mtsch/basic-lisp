{-# OPTIONS_GHC -Wall #-}
module Evaluator
    ( eval )
where

import           Data.IORef
import qualified Data.HashMap.Lazy as Map
import           Data.Maybe

import           SExpr
import           Reader

-- Evaluate an S-Expression.
eval :: Environment -> SExpr -> IO EnvSExpr
eval env expression =
    case expression of
      (Atom a) -> return (env, resolveVar a env)
      (List l) -> evalListBody env l
      value    -> return (env, value)

-- Evaluate the body of a list.
-- This can be a function call or a special form such as def or if-then-else.
evalListBody :: Environment -> [SExpr] -> IO EnvSExpr
evalListBody env expressions =
    case expressions of

      -- Errors propagate. The first term makes pattern matching exhausitve.
      err@(Err _) : _           -> return (env, err)
      _ | any isErr expressions -> return (env, head $ filter isErr expressions)

      -- Can't call an integer, a string, a bool, nil or an empty list.
      Int _    : _ -> return (env, Err "Can't call an int!")
      String _ : _ -> return (env, Err "Can't call a string!")
      Bool _   : _ -> return (env, Err "Can't call a bool!")
      Nil      : _ -> return (env, Err "Can't call nil!")
      List []  : _ -> return (env, Err "Can't call an empty list!")

      -- Special forms.
      Atom "def"   : args -> defSF env args
      Atom "if"    : args -> ifSF env args
      Atom "fun"   : args -> funSF env args
      Atom "eval"  : args -> evalSF env args
      Atom "quote" : args -> quoteSF env args
      Atom "set!"  : args -> setSF env args
      Atom "get!"  : args -> getSF env args
      Atom "do"    : args -> doSF env args

      -- Atom that needs to be resolved.
      Atom a : rest ->
          do resolved <- snd <$> eval env (Atom a)
             eval env $ List (resolved : rest)

      -- Primitive function call.
      Primitive p : args ->
          do result <- p =<< mapM (fmap snd . eval env) args
             return (env, result)

      -- Function call.
      fun@Fun {} : args ->
          callFunction env fun args

      -- Expression that needs to be evaluated.
      expr@(List _) : rest ->
          do (env', resolved) <- eval env expr
             eval env' $ List (resolved : rest)

      -- Empty list doesn't evaluate.
      [] -> return (env, List [])

-- Call a function.
callFunction :: Environment -> SExpr -> [SExpr] -> IO EnvSExpr
callFunction env fun args
    | not $ isFun fun             = error "Trying to call a non-function!"
    | length args /= length names = return (env, Err "Wrong airity!")
    | otherwise =
        do evaldArgs <- mapM (fmap snd . eval env) args
           let argEnv = Map.fromList $ ("recur", fun) : zip names evaldArgs
           let env'   = Env (Map.unions [argEnv, closure fun, scope env])
                            (refs env)
           result <- snd <$> eval env' (body fun)
           return (env, result)
    where
      names = argNames fun

-- Error message used in special forms.
sfError :: String -> SExpr
sfError name = Err $ "That's not how you use \"" ++ name ++ "\"!"

-- Variable definition.
defSF :: Environment -> [SExpr] -> IO EnvSExpr
defSF env expressions =
    case expressions of
      [Atom name, sexpr] ->
          do (_, rhs) <- eval env sexpr
             let env' = addVar name rhs env
             return (env', rhs)

      _ -> return (env, sfError "def")

-- if-then-else.
ifSF :: Environment -> [SExpr] -> IO EnvSExpr
ifSF env expressions =
    case expressions of
      [predicate, th, el] ->
          do evaldPred <- eval env predicate
             case evaldPred of
               (env', Bool False) -> eval env' el
               (env', List [])    -> eval env' el
               (env', Nil)        -> eval env' el
               (env', _)          -> eval env' th

      _ -> return (env, sfError "if")

-- Function declaration.
funSF :: Environment -> [SExpr] -> IO EnvSExpr
funSF env expressions =
    case expressions of
      [List args, List expr] ->
          return (env, if not . all isAtom $ args
                       then Err "Bad function declaration!"
                       else Fun (map unpackAtom args) (scope env) (List expr))

      _ -> return (env, sfError "fun")

-- Evaluate string or (quoted) list or atom.
evalSF :: Environment -> [SExpr] -> IO EnvSExpr
evalSF env expressions =
    case expressions of
      -- Evaluate string(s).
      args | all isString args ->
          eval env $ readSExpr $ concatMap unpackString args

      -- Evaluate quoted list.
      [List [Atom "quote", List l]] ->
          eval env (List l)

      -- Evaluate list.
      [list@(List _)] ->
          do result <- snd <$> eval env list
             eval env $ List [Atom "eval", result]

      -- Eval atom.
      [atom@(Atom _)] ->
          do result <- snd <$> eval env atom
             eval env $ List [Atom "eval", result]

      -- A value evals to itself.
      [val] -> return (env, val)

      -- Bad eval.
      _ -> return (env, sfError "eval")

-- Quoted terms do not evaluate.
quoteSF :: Environment -> [SExpr] -> IO EnvSExpr
quoteSF env expressions =
    case expressions of
      [val] -> return (env, val)
      _     -> return (env, sfError "quote")

-- Set a mutable reference.
setSF :: Environment -> [SExpr] -> IO EnvSExpr
setSF env expressions =
    case expressions of
      [Atom name, expression] ->
          do value <- snd <$> eval env expression
             modifyIORef' (refs env) (Map.insert name value)
             return (env, value)

      _ -> return (env, sfError "set!")

-- Get the value of a mutable reference.
getSF :: Environment -> [SExpr] -> IO EnvSExpr
getSF env expressions =
    case expressions of
      [Atom name] ->
          do ioRefs <- readIORef (refs env)
             let value = fromMaybe Nil (Map.lookup name ioRefs)
             return (env, value)

      -- Bad get.
      _ -> return (env, sfError "get!")

-- Evaluate a multiple S-Expressions in a sequential fashion.
-- Return the last result.
doSF :: Environment -> [SExpr] -> IO EnvSExpr
doSF env []       = return (env, Nil)
doSF env [ex]     = eval env ex
doSF env (ex:exs) = do env' <- fst <$> eval env ex
                       doSF env' exs
