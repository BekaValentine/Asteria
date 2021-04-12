module Parser.Names where

import Control.Monad
import Data.Char
import Text.Megaparsec

import Parser.Common
import Syntax.Names


-- [a-z][a-zA-Z0-9_]*
parseVarName :: AsteriaParser VarName
parseVarName =
  try $ do
    vn@(c:_) <- identifier
    guard (isLower c)
    return $ VarName vn

-- [A-Z][a-zA-Z0-9_]*
parseIdentName :: AsteriaParser IdentName
parseIdentName =
  try $ do
    vn@(c:_) <- identifier
    guard (isUpper c)
    return $ IdentName vn

parseModuleName :: AsteriaParser ModuleName
parseModuleName = ModuleName <$> parseIdentName

parseModulePath :: AsteriaParser ModulePath
parseModulePath =
  ModulePath <$> sepBy1 parseModulePathPart (symbol ".")
  where
    parseModulePathPart :: AsteriaParser ModuleName
    parseModulePathPart =
      do c <-  identStart
         cs <- many identLetter
         return $ ModuleName (IdentName (c:cs))

parseTypeVar :: AsteriaParser TypeVar
parseTypeVar = TypeVar <$> parseVarName

parseTypeName :: AsteriaParser TypeName
parseTypeName = TypeName <$> parseIdentName

parseClassName :: AsteriaParser ClassName
parseClassName = ClassName <$> parseIdentName

parseTermVar :: AsteriaParser TermVar
parseTermVar = TermVar <$> parseVarName

parseTermName :: AsteriaParser TermName
parseTermName = TermName <$> parseIdentName
