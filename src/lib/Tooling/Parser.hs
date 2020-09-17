module Tooling.Parser where

import Options
import Text.Parsec.Error (errorMessages, messageString)

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
                 Left err -> mapM_ (putStrLn . messageString) (errorMessages err)
                 Right _ -> do putStrLn ""
                               putStrLn "Parsing complete. No errors."
                               putStrLn ""
                               putStrLn "------------------"
                               putStrLn ""
                               putStr src
                               putStrLn ""
                               putStrLn "------------------"
                               putStrLn ""
