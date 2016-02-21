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

-- The basic starting scope.
startScope :: Scope
startScope = primitiveEnv

-- A prompt that prints, flushes and reads.
prompt :: String -> IO String
prompt p = do putStr p
              hFlush stdout
              getLine

-- The REPL recursively reads from input, evaluates and prints the result.
repl :: Environment -> IO ()
repl env = do input <- prompt "=> "
              let ast = readSExpr (input ++ "\n")
              if isErr ast
                then print ast >> repl env
                else do (env', res) <- eval env ast
                        print res
                        repl env'

-- main creates an environment and calls repl.
main :: IO ()
main = do ioRefs <- newIORef Map.empty
          repl (Env startScope ioRefs)
