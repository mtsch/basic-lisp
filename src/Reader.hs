{-# OPTIONS_GHC -Wall #-}
module Reader
    ( readSExpr )
where

import Text.Parsec hiding (spaces, space)

import SExpr

-- Allowed symbols.
symbol :: Parsec String () Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~."

-- Whitespace. ',' is also treated as whitespace.
space :: Parsec String () Char
space = oneOf " \t\n\r,"

-- Skip comment
comment :: Parsec String () ()
comment = (do _ <- char ';'
              skipMany $ noneOf "\n"
              _ <- char '\n'
              return ()) <?> ""

-- Skip spaces.
spaces :: Parsec String () ()
spaces = skipMany space >> skipMany comment

-- Parse an atom.
atom :: Parsec String () SExpr
atom = do first <- symbol <|> letter
          rest  <- many (symbol <|> alphaNum)
          let name = first : rest
          return $ case name of
                     "true"  -> Bool True
                     "false" -> Bool False
                     "nil"   -> Nil
                     _       -> Atom name

-- Parse a container delimited by l and r.
-- l and r can't start or end with valid symbols!
container :: (String, String) -> Parsec String () [SExpr]
container (l, r) = do spaces
                      _ <- string l
                      spaces
                      contents <- expr `sepEndBy` spaces
                      _ <- string r
                      spaces
                      return contents

-- Parse a list.
list :: Parsec String () SExpr
list = List <$> container ("(", ")")

-- Parse a quoted list.
quotList :: Parsec String () SExpr
quotList = List <$> (container ("'(", ")") >>= \l ->
                         return $ Atom "quote" : [List l])

-- Parse a number.
int :: Parsec String () SExpr
int = (Int . read) <$> many1 digit

-- Parse a string.
str :: Parsec String () SExpr
str = do _ <- char '"'
         value <- many $ noneOf "\""
         _ <- char '"'
         (return . String) value

-- Parse an expression.
expr :: Parsec String () SExpr
expr = quotList <|> list <|> atom <|> int <|> str <?> "expression"

-- Read S-Expression from string.
readSExpr :: String -> [SExpr]
readSExpr text = either (\e -> [Err $ "Parse error in " ++ show e])
                        id (parse exprs "" text)
    where
      exprs = do spaces
                 expr `sepEndBy` spaces
