module Main where

import Lexer
import Parser
import CodInt
import CodMaq
import           Data.Map (Map)
import qualified Data.Map as Map
import           Control.Monad.State

main :: IO ()
main = do
  txt <- getContents
  putStrLn ""
  
  print "Arvore Sintatica:"
  print (parser $ alexScanTokens txt)
 
  putStrLn ""
  
  print "Codigo Intermedio:"
  print (printCodInt(parser $ alexScanTokens txt))
  
  putStrLn ""
  
  print "Codigo Maquina:"
  putStrLn (readInstList(printCodInt(parser $ alexScanTokens txt)))