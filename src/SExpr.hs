{-# OPTIONS_GHC -Wall #-}
module SExpr
    ( Environment (..)
    , Scope
    , addVar
    , resolveVar
    , EnvSExpr
    , SExpr (..)
    , isAtom
    , isList
    , isInt
    , isString
    , isBool
    , isErr
    , isFun
    , isValue
    , unpackInt
    , unpackString
    , unpackList
    , unpackAtom )
where

import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as Map
import           Data.Maybe

import           Data.IORef

-- The scope is a mapping from names to expressions.
type Scope = HashMap String SExpr

-- An environment is scope and a reference to the mutable variables.
data Environment = Env { scope :: Scope
                       , refs  :: IORef (Scope) }

-- Add variable to environment.
addVar :: String -> SExpr -> Environment -> Environment
addVar name value (Env {scope = s, refs = r}) =
    Env { scope = Map.insert name value s
        , refs  = r }

-- Find variable in environment.
resolveVar :: String -> Environment -> SExpr
resolveVar name env = fromMaybe (Err $ "Unknown symbol \"" ++ name ++ "\"!")
                                (Map.lookup name $ scope env)

-- Environment, value pair.
type EnvSExpr = (Environment, SExpr)

-- S-expression A.K.A. the AST.
data SExpr = Atom String
           | List [SExpr]
           | Int Int
           | String String
           | Bool Bool
           | Nil
           | Primitive ([SExpr] -> IO SExpr)
           | Err String
           | Fun { argNames :: [String]
                 , closure  :: Scope
                 , body     :: SExpr }

instance Eq SExpr where
    Atom a1    == Atom a2    = a1 == a2
    List l1    == List l2    = l1 == l2
    Int i1     == Int i2 = i1 == i2
    String s1  == String s2  = s1 == s2
    Bool b1    == Bool b2    = b1 == b2
    Nil        == Nil        = True
    _          == _          = False

instance Show SExpr where
    show (Atom a)       = a
    show (List l)       = "(" ++ (unwords . map show) l ++ ")"
    show (Int i)        = show i
    show (String s)     = "\"" ++ s ++ "\""
    show (Bool True)    = "true"
    show (Bool False)   = "false"
    show Nil            = "nil"
    show (Primitive _)  = "<primitive>"
    show (Err e)        = "Error: " ++ e
    show (Fun args _ _) = "(fun (" ++ unwords args ++ ") ...)"

-- Check types of SExprs.
isAtom :: SExpr -> Bool
isAtom (Atom _) = True
isAtom _        = False

isList :: SExpr -> Bool
isList (List _) = True
isList _        = False

isInt :: SExpr -> Bool
isInt (Int _) = True
isInt _       = False

isString :: SExpr -> Bool
isString (String _) = True
isString _          = False

isBool :: SExpr -> Bool
isBool (Bool _) = True
isBool _        = False

isErr :: SExpr -> Bool
isErr (Err _) = True
isErr _       = False

isFun :: SExpr -> Bool
isFun Fun {} = True
isFun _      = False

isValue :: SExpr -> Bool
isValue v = isInt v || isString v || isBool v

-- Unpack int from SExpr.
unpackInt :: SExpr -> Int
unpackInt (Int i) = i
unpackInt _       = error "Can't unpack this to Int"

-- Unpack string from SExpr.
unpackString :: SExpr -> String
unpackString (String s) = s
unpackString _          = error "Can't unpack this as String!"

-- Unpack list from SExpr.
unpackList :: SExpr -> [SExpr]
unpackList (List l) = l
unpackList _        = error "Can't unpack this as List!"

-- Unpack string from Atom.
unpackAtom :: SExpr -> String
unpackAtom (Atom a) = a
unpackAtom _        = error "Can't unpack this as Atom!"
