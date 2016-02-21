{-# OPTIONS_GHC -Wall #-}
module Main
where

import qualified Data.HashMap.Lazy as Map
import           Data.IORef
import           System.IO

import           Evaluator
import           Reader
import           Primitives
import           SExpr

startScope :: Scope
startScope = primitiveEnv

main :: IO ()
main = do ioRefs <- newIORef Map.empty
          repl (Env startScope ioRefs)

prompt :: String -> IO String
prompt p = do putStr p
              hFlush stdout
              getLine

-- The REPL
repl :: Environment -> IO ()
repl env = do input <- prompt "=> "
              let ast = readSExpr (input ++ "\n")
              if isErr ast
                then print ast >> repl env
                else do (env', res) <- eval env ast
                        print res
                        repl env'
