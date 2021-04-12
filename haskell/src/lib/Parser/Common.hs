module Parser.Common where

import Control.Monad.Identity
import Data.Text
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L






type AsteriaParser = Parsec Void String
type AsteriaParseErrors = ParseErrorBundle String Void

runAsteriaParser :: AsteriaParser a -> String -> Either AsteriaParseErrors a
runAsteriaParser p = parse (p <* eof) "(no source)"

spaceConsumer :: AsteriaParser ()
spaceConsumer = L.space
  (void (some (char ' ' <|> char '\t')))
  (L.skipLineComment "--")
  (L.skipBlockCommentNested "{-" "-}")

newlineSpaceConsumer :: AsteriaParser ()
newlineSpaceConsumer = L.space
  space1
  (L.skipLineComment "--")
  (L.skipBlockCommentNested "{-" "-}")


symbol :: String -> AsteriaParser String
symbol    = L.symbol spaceConsumer

parens    = between (symbol "(") (symbol ")")
braces    = between (symbol "{") (symbol "}")
angles    = between (symbol "<") (symbol ">")
brackets  = between (symbol "[") (symbol "]")
semicolon = symbol ";"
comma     = symbol ","
colon     = symbol ":"
dot       = symbol "."

identStart :: AsteriaParser Char
identStart = letterChar <|> char '_'

identLetter :: AsteriaParser Char
identLetter = alphaNumChar <|> char '_'

identifier :: AsteriaParser String
identifier = L.lexeme spaceConsumer content
  where
    content =
      do c <- identStart
         cs <- many identLetter
         guard (not ((c:cs) `elem` reservedNames))
         return (c:cs)

braceBlock :: String -> AsteriaParser a -> AsteriaParser [a]
braceBlock sep p = braces (sepBy p (symbol sep))

{-
indentedBlock :: AsteriaParser a -> AsteriaParser [a]
indentedBlock p = L.indentBlock newlineSpaceConsumer (return (L.IndentSome Nothing return p))

topLevel :: AsteriaParser a -> AsteriaParser a
topLevel = L.nonIndented newlineSpaceConsumer
-}

reservedNames = [
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

{-}
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

-}
