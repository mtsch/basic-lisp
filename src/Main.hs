{-# OPTIONS_GHC -Wall #-}
module Main
where

import qualified Data.HashMap.Lazy as Map
import           Data.IORef

import           Evaluator
import           Reader
import           Primitives
import           SExpr

startScope :: Scope
startScope = primitiveEnv

main :: IO ()
main = do ioRefs <- newIORef Map.empty
          mainLoop (Env startScope ioRefs)

-- The REPL
mainLoop :: Environment -> IO ()
mainLoop env = do putStr "=> "
                  input <- getLine
                  let ast = readSExpr (input ++ "\n")
                  if any isErr ast
                    then (print . head . filter isErr) ast >> mainLoop env
                    else do (env', res) <- evalMulti env ast
                            print res
                            mainLoop env'
