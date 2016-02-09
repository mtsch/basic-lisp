module Primitives
    ( primitiveEnv )
where

import qualified Data.HashMap.Lazy as Map

import           SExpr

airityError :: SExpr
airityError = Err "Wrong airity!"

-- Two argument fun on Int to two or more argument primitive
intintPrimitive :: (Int -> Int -> Int) -> [SExpr] -> SExpr
intintPrimitive _ []       = airityError
intintPrimitive _ [_]      = airityError
intintPrimitive fun (e:es) =
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

headPrimitive :: [SExpr] -> SExpr
headPrimitive [List []] = Err "Function expecting non-empty list!"
headPrimitive [List l]  = head l
headPrimitive [_]       = Err "Function expecting list!"
headPrimitive _         = airityError

tailPrimitive :: [SExpr] -> SExpr
tailPrimitive [List []] = Err "Function expecting non-empty list!"
tailPrimitive [List l] = List $ tail l
tailPrimitive [_]      = Err "Function expecting list!"
tailPrimitive _        = airityError

consPrimitive :: [SExpr] -> SExpr
consPrimitive [val, List l] = List $ val : l
consPrimitive [_, _]        = Err "Function expecting value and list!"
consPrimitive _             = airityError

-- Environment containing primitives.
primitiveEnv :: Environment
primitiveEnv = Map.fromList [ ("+",    Primitive $ intintPrimitive (+))
                            , ("-",    Primitive $ intintPrimitive (-))
                            , ("*",    Primitive $ intintPrimitive (*))
                            , ("div",  Primitive $ intintPrimitive div)
                            , ("rem",  Primitive $ intintPrimitive rem)
                            , ("=",    Primitive $ comparisonFun (==))
                            , ("not=", Primitive $ comparisonFun (/=))
                            , ("head", Primitive $ headPrimitive)
                            , ("tail", Primitive $ tailPrimitive)
                            , ("cons", Primitive $ consPrimitive)
                            ]
