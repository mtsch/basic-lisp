module Reader
    ( readSExpr )
where

import           Control.Applicative ((<$>))
import           Text.Parsec hiding (spaces, space)

import           SExpr

-- Allowed symbols.
symbol :: Parsec String () Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~."

-- Whitespace. ',' is also treated as whitespace.
space :: Parsec String () Char
space = oneOf " \t\n\r,"

-- Skip comment
comment :: Parsec String () ()
comment = (do char ';'
              skipMany $ noneOf "\n"
              char '\n'
              return ()) <?> ""

-- Skip spaces.
spaces :: Parsec String () ()
spaces = skipMany space >> skipMany comment

-- Parse an atom.
atom :: Parsec String () SExpr
atom = do first <- symbol <|> letter
          rest  <- many (symbol <|> alphaNum)
          let atom = first : rest
          return $ case atom of
                     "true"  -> Bool True
                     "false" -> Bool False
                     "nil"   -> Nil
                     _       -> Atom atom

-- Parse a container delimited by l and r.
-- l and r can't start or end with valid symbols!
container :: (String, String) -> Parsec String () [SExpr]
container (l, r) = do spaces
                      string l
                      spaces
                      contents <- expr `sepEndBy` spaces
                      string r
                      spaces
                      return contents

-- Parse a list.
list :: Parsec String () SExpr
list = List <$> container ("(", ")")

-- Parse a quoted list.
quotList :: Parsec String () SExpr
quotList = List <$> (container ("'(", ")") >>= \l ->
                         return $ (Atom "quote") : [List l])

-- Parse a number.
int :: Parsec String () SExpr
int = (Integer. read) <$> many1 digit

-- Parse a string.
str :: Parsec String () SExpr
str = do char '"'
         str <- many $ noneOf "\""
         char '"'
         (return . String) str

-- Parse an expression.
expr :: Parsec String () SExpr
expr = quotList <|> list <|> atom <|> int <|> str <?> "expression"

-- Read S-Expression from string.
readSExpr :: String -> [SExpr]
readSExpr text = either (\e -> [Err (show e)]) id (parse exprs "" text)
    where
      exprs = do spaces
                 expr `sepEndBy` spaces
