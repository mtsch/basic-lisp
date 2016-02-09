module Primitives
    ( primitiveEnv )
where

import qualified Data.HashMap.Lazy as Map

import           SExpr

airityError :: SExpr
airityError = Err "Wrong airity!"

expectingError :: String -> SExpr
expectingError s = Err $ "Function expecting " ++ s ++ "!"

-- Two argument fun on Int to two or more argument primitive
intintPrim :: (Int -> Int -> Int) -> [SExpr] -> SExpr
intintPrim _ []       = airityError
intintPrim _ [_]      = airityError
intintPrim fun (e:es) =
    case e of
      (Integer i) -> aux i es
          where
            aux acc exs =
                case exs of
                  []           -> Integer acc
                  (first:rest) -> case first of
                                    (Integer x) -> let acc' = fun acc x
                                                   in aux acc' rest
                                    _ -> expectingError "two integers"
      _ -> expectingError "two integers"

-- Equal an not equal function wrapper.
eqneqPrim :: (SExpr -> SExpr -> Bool) -> [SExpr] -> SExpr
eqneqPrim fun [exp1, exp2] = Bool $ fun exp1 exp2
eqneqPrim _ _              = airityError

intComparisonPrim :: (Int -> Int -> Bool) -> [SExpr] -> SExpr
intComparisonPrim fun [Integer i1, Integer i2] = Bool $ fun i1 i2
intComparisonPrim _ [_, _]                     = expectingError "two integers"
intComparisonPrim _ _                          = airityError

-- List head.
headPrim :: [SExpr] -> SExpr
headPrim [List []] = expectingError "non-empty list"
headPrim [List l]  = head l
headPrim [_]       = expectingError "list"
headPrim _         = airityError

-- List tail.
tailPrim :: [SExpr] -> SExpr
tailPrim [List []] = expectingError "non-empty list"
tailPrim [List l]  = List $ tail l
tailPrim [_]       = expectingError "list"
tailPrim _         = airityError

-- Cons for lists.
consPrim :: [SExpr] -> SExpr
consPrim [val, List l] = List $ val : l
consPrim [_, _]        = expectingError "value and list"
consPrim _             = airityError

-- Environment containing primitives.
primitiveEnv :: Environment
primitiveEnv = Map.fromList [ ("+",    Primitive $ intintPrim (+))
                            , ("-",    Primitive $ intintPrim (-))
                            , ("*",    Primitive $ intintPrim (*))
                            , ("div",  Primitive $ intintPrim div)
                            , ("rem",  Primitive $ intintPrim rem)
                            , ("=",    Primitive $ eqneqPrim (==))
                            , ("not=", Primitive $ eqneqPrim (/=))
                            , (">",    Primitive $ intComparisonPrim (>))
                            , (">=",   Primitive $ intComparisonPrim (>=))
                            , ("<",    Primitive $ intComparisonPrim (<))
                            , ("<=",   Primitive $ intComparisonPrim (<=))
                            , ("head", Primitive $ headPrim)
                            , ("tail", Primitive $ tailPrim)
                            , ("cons", Primitive $ consPrim)
                            ]
