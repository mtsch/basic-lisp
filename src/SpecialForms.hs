module SpecialForms
where

import Evaluator
import SExpr



-- Error message used in special forms.
sfError :: String -> SExpr
sfError name = Err $ "That's not how you use \"" ++ name ++ "\"!"

-- Variable definition.
defSF :: Environment -> [SExpr] -> IO EnvSExpr
defSF env expressions =
    case expressions of
      [Atom "def", Atom name, sexpr] ->
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
          evalMulti env $ concatMap (readSExpr . unpackString) args

      -- Evaluate quoted list.
      [List [Atom "quote", List l]] ->
          evalListBody env l

      -- Evaluate list.
      [List l] ->
          evalListBody env l

      -- Eval atom.
      [atom@(Atom _)] ->
           do result <- snd <$> eval env atom
              evalListBody env [Atom "eval", result]

      -- Bad eval.
      _ -> return (env, Err "Can't eval that!")

-- Use quote to make lists that don't evaluate.
quoteSF :: Environment -> [SExpr] -> IO EnvSExpr
quoteSF env expressions =
    case expressions of
      [list@(List _)] ->
          return (env, list)

      _ -> return (env, sfError "quote")

-- Set mutable reference.
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
