{-# OPTIONS_GHC -Wall #-}
module Reader
    ( readSExpr )
where

import Control.Applicative
import Text.Parsec hiding (spaces, space, (<|>), many)

import SExpr

-- Allowed symbols.
symbol :: Parsec String () Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~."

-- Whitespace. ',' is also treated as whitespace.
space :: Parsec String () Char
space = oneOf " \t\n\r,"

-- Skip comment
comment :: Parsec String () ()
comment = (char ';' *> skipMany (noneOf "\n") *> char '\n' *> pure ()) <?> ""

-- Skip spaces.
spaces :: Parsec String () ()
spaces = skipMany space *> skipMany comment

-- Parse an atom.
atom :: Parsec String () SExpr
atom = liftA pick $ (:) <$> (symbol <|> letter) <*> many (symbol <|> alphaNum)
       where
         pick name =
             case name of
               "true"  -> Bool True
               "false" -> Bool False
               "nil"   -> Nil
               _       -> Atom name

-- Parse a container separated by l and r
container :: String -> String -> Parsec String () [SExpr]
container l r = left *> expr `sepEndBy` spaces <* right
    where
      left  = spaces *> string l *> spaces
      right = spaces <* string r <* spaces

-- Parse a list.
list :: Parsec String () SExpr
list = List <$> container "(" ")"

-- Parse a quoted list.
quotList :: Parsec String () SExpr
quotList = liftA (\l -> List $ Atom "quote" : [List l]) $ container "'(" ")"

-- Parse a number.
int :: Parsec String () SExpr
int = (Int . read) <$> many1 digit

str :: Parsec String () SExpr
str = liftA String $ char '"' *> many (noneOf "\"") <* char '"'

-- Parse an expression.
expr :: Parsec String () SExpr
expr = ex <* (optionMaybe eof <?> "") <?> "expression"
    where
      ex = quotList <|> list <|> atom <|> int <|> str

-- Read S-Expression from string.
-- It works by reading a sequence of instructions into a do-block.
readSExpr :: String -> SExpr
readSExpr text = either (\e -> Err $ "Parse error in " ++ show e)
                        (\e -> List $ Atom "do" : e)
                        (parse exprs "" text)
    where
      exprs = spaces *> expr `sepEndBy` spaces
