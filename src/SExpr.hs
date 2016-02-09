module SExpr
    ( Environment
    , resolveName
    , EnvSExpr
    , SExpr (..)
    , isAtom
    , isInteger
    , isBool
    , isErr
    , isFun )
where

import           Data.Foldable (toList)
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as Map
import           Data.Maybe
import           Text.Parsec hiding (spaces, space)

-- An environment is a mapping from names to expressions.
type Environment = HashMap String SExpr

-- Find variable in environment.
resolveName :: Environment -> String -> SExpr
resolveName env name = fromMaybe Nil (Map.lookup name env)


-- Environment, value pair.
type EnvSExpr = (Environment, SExpr)

-- S-expression A.K.A. the AST.
data SExpr = Atom String
           | List [SExpr]
           | Integer Int
           | String String
           | Bool Bool
           | Nil
           | Primitive ([SExpr] -> SExpr)
           | Err String
           | Fun { argNames :: [String]
                 , closure  :: Environment
                 , body     :: SExpr }

instance Eq SExpr where
    Atom a1    == Atom a2    = a1 == a2
    List l1    == List l2    = l1 == l2
    Integer i1 == Integer i2 = i1 == i2
    String s1  == String s2  = s1 == s2
    Bool b1    == Bool b2    = b1 == b2
    Nil        == Nil        = True
    _          == _          = False

instance Show SExpr where
    show (Atom a)       = a
    show (List l)       = "(" ++ (unwords . map show) l ++ ")"
    show (Integer i)    = show i
    show (String s)     = "\"" ++ s ++ "\""
    show (Bool True)    = "true"
    show (Bool False)   = "false"
    show Nil            = "nil"
    show (Primitive _)  = "<primitive>"
    show (Err e)        = "Error: " ++ e
    show (Fun args _ _) = "(fn (" ++ unwords args ++ ") ...)"

-- Check types of SExprs.
isAtom :: SExpr -> Bool
isAtom (Atom _) = True
isAtom _        = False

isInteger :: SExpr -> Bool
isInteger (Integer _) = True
isInteger _           = False

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
