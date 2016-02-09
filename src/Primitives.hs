module Primitives
    ( primitiveEnv )
where

import qualified Data.HashMap.Lazy as Map

import           Evaluator
import           SExpr

-- Error thrown when the number of arguments to a functions is wrong.
airityError :: IO SExpr
airityError = (return . Err) "Wrong airity!"

-- Type error.
expectingError :: String -> IO SExpr
expectingError s = return . Err $ "Function expecting " ++ s ++ "!"

-- Int -> Int -> Int function wrapped into a primitive function.
intintPrim :: (Int -> Int -> Int) -> [SExpr] -> IO SExpr
intintPrim fun args
    | length args < 2    = airityError
    | all isInteger args = return . Integer $ foldl1 fun (map unpackInteger args)
    | otherwise          = expectingError "integers"

-- Integer division.
divPrim :: [SExpr] -> IO SExpr
divPrim args
    | all isInteger args &&
      notElem (Integer 0) args = return . Integer $ foldl1 div (map unpackInteger args)
    | all isInteger args       = (return . Err) "Division by zero!"
    | otherwise                = expectingError "integers"

-- Equal and not equal function wrapper.
eqneqPrim :: (SExpr -> SExpr -> Bool) -> [SExpr] -> IO SExpr
eqneqPrim fun [exp1, exp2] = return . Bool $ fun exp1 exp2
eqneqPrim _ _              = airityError

-- Wrapper for functions that compare ints.
intComparisonPrim :: (Int -> Int -> Bool) -> [SExpr] -> IO SExpr
intComparisonPrim fun [Integer i1, Integer i2] = return . Bool $ fun i1 i2
intComparisonPrim _ [_, _]                     = expectingError "two integers"
intComparisonPrim _ _                          = airityError

-- List head.
headPrim :: [SExpr] -> IO SExpr
headPrim [List []] = expectingError "non-empty list"
headPrim [List l]  = return (head l)
headPrim [_]       = expectingError "list"
headPrim _         = airityError

-- List tail.
tailPrim :: [SExpr] -> IO SExpr
tailPrim [List []] = expectingError "non-empty list"
tailPrim [List l]  = return . List $ tail l
tailPrim [_]       = expectingError "list"
tailPrim _         = airityError

-- Cons for lists.
consPrim :: [SExpr] -> IO SExpr
consPrim [val, List l] = return . List $ val : l
consPrim [_, _]        = expectingError "value and list"
consPrim _             = airityError

-- Concat/paste strings or lists.
concatPrim :: [SExpr] -> IO SExpr
concatPrim args
    | length args < 2   = airityError
    | all isString args = return . String $ foldl1 (++) $ map unpackString args
    | all isList args   = return . List   $ foldl1 (++) $ map unpackList args
    | otherwise         = expectingError "strings or lists"

-- Throw an error.
throwPrim :: [SExpr] -> IO SExpr
throwPrim [String message] = (return . Err) message
throwPrim [_]              = expectingError "string"
throwPrim _                = airityError

-- Print to stdout.
printPrim :: [SExpr] -> IO SExpr
printPrim args = putStrLn (concatMap show args) >> return Nil

-- Read string from stdin. TODO
readPrim :: [SExpr] -> IO SExpr
readPrim [] = fmap String getLine
readPrim _  = airityError

-- Environment containing primitives.
primitiveEnv :: Environment
primitiveEnv = Map.fromList [ ("+",      Primitive $ intintPrim (+))
                            , ("-",      Primitive $ intintPrim (-))
                            , ("*",      Primitive $ intintPrim (*))
                            , ("div",    Primitive divPrim)
                            , ("rem",    Primitive $ intintPrim rem)
                            , ("=",      Primitive $ eqneqPrim (==))
                            , ("not=",   Primitive $ eqneqPrim (/=))
                            , (">",      Primitive $ intComparisonPrim (>))
                            , (">=",     Primitive $ intComparisonPrim (>=))
                            , ("<",      Primitive $ intComparisonPrim (<))
                            , ("<=",     Primitive $ intComparisonPrim (<=))
                            , ("head",   Primitive headPrim)
                            , ("tail",   Primitive tailPrim)
                            , ("cons",   Primitive consPrim)
                            , ("concat", Primitive concatPrim)
                            , ("throw",  Primitive throwPrim)
                            , ("print",  Primitive printPrim)
                            , ("read",   Primitive printPrim)
                            ]
