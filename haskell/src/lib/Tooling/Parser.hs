module Tooling.Parser where

import Data.List.NonEmpty
import Options
import Text.Megaparsec.Error

import Parser.Common
import Parser.Module

data ParserOptions = ParserOptions
                     { sourceFile :: String
                     }

instance Options ParserOptions where
   defineOptions =
         pure ParserOptions
     <*> simpleOption "source" "" "The source file to parse."

main :: IO ()
main =
  runCommand $ \opts args ->
    let file = sourceFile opts
    in if file == ""
       then putStrLn "\nNo source file given.\n"
       else do src <- readFile file
               let res = runAsteriaParser parseRawModule src
               case res of
                 Left errb -> putStrLn (errorBundlePretty errb)
                 Right m -> do putStrLn ""
                               putStrLn "Parsing complete. No errors."
                               putStrLn ""
                               putStrLn "------------------"
                               putStrLn ""
                               putStr src
                               putStrLn ""
                               putStrLn "------------------"
                               putStrLn ""
                               putStrLn (show m)
