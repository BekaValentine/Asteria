module Parser.Common where

import Control.Monad.Identity
import Text.Parsec
import Text.Parsec.Indent
import qualified Text.Parsec.Token as Token






type AsteriaParser a = IndentParser String () a

runAsteriaParser :: AsteriaParser a -> String -> Either ParseError a
runAsteriaParser p = runIndentParser (p <* eof) () "(no source)"



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
                    , "define"
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

identifier :: AsteriaParser String
identifier = Token.identifier tokenParser

reserved :: String -> AsteriaParser ()
reserved = Token.reserved tokenParser

reservedOp :: String -> AsteriaParser ()
reservedOp = Token.reservedOp tokenParser

parens :: AsteriaParser a -> AsteriaParser a
parens = Token.parens tokenParser

braces :: AsteriaParser a -> AsteriaParser a
braces = Token.braces tokenParser

symbol :: String -> AsteriaParser String
symbol = Token.symbol tokenParser

whiteSpace :: AsteriaParser ()
whiteSpace = Token.whiteSpace tokenParser
