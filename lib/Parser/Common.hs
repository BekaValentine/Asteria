module Parser.Common where

import Control.Monad.Identity
import Text.Parsec
import Text.Parsec.Indent
import qualified Text.Parsec.Token as Token





type YAPLParser a = IndentParser String () a



languageDef :: Token.GenLanguageDef String () (IndentT Identity)
languageDef = Token.LanguageDef
                { Token.commentStart = "{-"
                , Token.commentEnd = "-}"
                , Token.commentLine = "--"
                , Token.nestedComments = True
                , Token.identStart = letter <|> char '_'
                , Token.identLetter = alphaNum <|> char '_'
                , Token.opStart = oneOf ""
                , Token.opLetter = oneOf ""
                , Token.reservedNames =
                    [
                      -- multiple
                      "where"

                      -- modules
                    , "module"
                    , "import"
                    , "as"
                    , "using"
                    , "hiding"
                    , "to"

                      -- declarations
                    , "data"
                    , "type"
                    , "class"

                      -- terms
                    , "case"
                    , "of"
                    , "let"
                    , "in"

                      -- types
                    , "forall"
                    ]
                , Token.reservedOpNames =
                    [ "="
                    , "<="
                    , ":"
                    , "->"
                    , "\\"
                    , "@"
                    , "=>"
                    , "."
                    ]
                , Token.caseSensitive = True
                }

tokenParser :: Token.GenTokenParser String () (IndentT Identity)
tokenParser = Token.makeTokenParser languageDef

identifier :: YAPLParser String
identifier = Token.identifier tokenParser

reserved :: String -> YAPLParser ()
reserved = Token.reserved tokenParser

reservedOp :: String -> YAPLParser ()
reservedOp = Token.reservedOp tokenParser

parens :: YAPLParser a -> YAPLParser a
parens = Token.parens tokenParser

symbol :: String -> YAPLParser String
symbol = Token.symbol tokenParser

whiteSpace :: YAPLParser ()
whiteSpace = Token.whiteSpace tokenParser
