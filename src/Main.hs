module Main
where

import           Control.Exception.Base
import qualified Data.HashMap.Lazy as Map
import           System.IO

import           Evaluator
import           Reader
import           Primitives
import           SExpr

startEnv :: Environment
startEnv = primitiveEnv

-- The REPL
mainLoop :: Environment -> IO ()
mainLoop env = do input <- getLine
                  let ast = readSExpr (input ++ "\n")
                  if any isErr ast
                    then (print . head . filter isErr) ast >> mainLoop env
                    else do (env', res) <- evalMulti env ast
                            print res
                            mainLoop env'

main :: IO ()
main = mainLoop startEnv
