module Parser.Pattern where

import Control.Applicative

import Parser.Common
import Parser.Names
import Syntax.Pattern



-- Pattern Pat ::=  x         (Variable Pattern)
--               |  Cn APat*  (Constructor Pattern)
parsePattern :: AsteriaParser Pattern
parsePattern =
      parseVariablePattern
  <|> parseConstructorPattern
  <|> parens parsePattern

parseVariablePattern :: AsteriaParser Pattern
parseVariablePattern =
  VarPat <$> parseTermVar

parseConstructorPattern :: AsteriaParser Pattern
parseConstructorPattern =
  ConPat <$> parseTermName <*> many parseArgPattern

parseConstructorPatternNoArguments :: AsteriaParser Pattern
parseConstructorPatternNoArguments =
  ConPat <$> parseTermName <*> return []

-- ArgPattern APat ::=  Pat     (Normal Pattern Argument)
--                   | {TyVar}  (Type Instantiation Argument)
parseArgPattern :: AsteriaParser ArgPattern
parseArgPattern =
      parseInstPattern
  <|> parseNormalPattern

parseInstPattern :: AsteriaParser ArgPattern
parseInstPattern =
  InstPat <$> braces parseTypeVar

parseNormalPattern :: AsteriaParser ArgPattern
parseNormalPattern =
  NormalPat <$> parsePattern
