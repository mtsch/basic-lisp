{-# OPTIONS_GHC -Wall #-}
module Primitives
    ( primitiveEnv )
where

import           Control.Exception.Base
import qualified Data.HashMap.Lazy as Map

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
    | length args < 2 = airityError
    | all isInt args  = return . Int $ foldl1 fun (map unpackInt args)
    | otherwise       = expectingError "integers"

-- Int division.
divPrim :: [SExpr] -> IO SExpr
divPrim args
    | all isInt args &&
      notElem (Int 0) args = return . Int $ foldl1 div (map unpackInt args)
    | all isInt args       = (return . Err) "Division by zero!"
    | otherwise            = expectingError "integers"

-- Equal and not equal function wrapper.
eqneqPrim :: (SExpr -> SExpr -> Bool) -> [SExpr] -> IO SExpr
eqneqPrim fun [exp1, exp2] = return . Bool $ fun exp1 exp2
eqneqPrim _ _              = airityError

-- Wrapper for functions that compare ints.
intComparisonPrim :: (Int -> Int -> Bool) -> [SExpr] -> IO SExpr
intComparisonPrim fun [Int i1, Int i2] = return . Bool $ fun i1 i2
intComparisonPrim _ [_, _]             = expectingError "two integers"
intComparisonPrim _ _                  = airityError

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

stringPrim :: [SExpr] -> IO SExpr
stringPrim = return . String . unwords . map show

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

-- Read a file.
slurpPrim :: [SExpr] -> IO SExpr
slurpPrim [String filename] = catch (fmap String (readFile filename))
                                    (openFileHandler filename)

slurpPrim [_] = expectingError "string"
slurpPrim _   = airityError

-- Error handler for slurpPrim.
openFileHandler :: String -> IOError -> IO SExpr
openFileHandler name _ = return . Err $ "File \"" ++ name ++ "\" not found!"

-- Environment containing primitive functions.
primitiveEnv :: Scope
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
                            , ("string", Primitive $ stringPrim)
                            , ("throw",  Primitive throwPrim)
                            , ("print",  Primitive printPrim)
                            , ("read",   Primitive readPrim)
                            , ("slurp",  Primitive slurpPrim)
                            ]
