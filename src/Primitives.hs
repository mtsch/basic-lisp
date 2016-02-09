module Primitives
    ( primitiveEnv )
where

import qualified Data.HashMap.Lazy as Map

import           SExpr

airityError :: SExpr
airityError = Err "Wrong airity!"

-- Two argument fun on Int to two or more argument primitive
intPrimitive :: (Int -> Int -> Int) -> [SExpr] -> SExpr
intPrimitive _ []       = airityError
intPrimitive _ [_]      = airityError
intPrimitive fun (e:es) =
    case e of
      (Integer i) -> aux i es
          where
            aux acc exs =
                case exs of
                  []           -> Integer acc
                  (first:rest) -> case first of
                                    (Integer x) -> let acc' = fun acc x
                                                   in aux acc' rest
                                    _ -> Err "Function expecting integer args!"
      _ -> Err "Function expecting integer args!"

-- Binary comparison functions
comparisonFun :: (SExpr -> SExpr -> Bool) -> [SExpr] -> SExpr
comparisonFun fun [exp1, exp2] = Bool $ fun exp1 exp2
comparisonFun _ _              = airityError

-- Environment containing primitives.
primitiveEnv :: Environment
primitiveEnv = Map.fromList [ ("+",    Primitive $ intPrimitive (+))
                            , ("-",    Primitive $ intPrimitive (-))
                            , ("*",    Primitive $ intPrimitive (*))
                            , ("div",  Primitive $ intPrimitive div)
                            , ("rem",  Primitive $ intPrimitive rem)
                            , ("=",    Primitive $ comparisonFun (==))
                            , ("not=", Primitive $ comparisonFun (/=))
                            ]
