module Parser.Module where

import Text.Megaparsec
import Control.Monad.Combinators

import Parser.Common
import Parser.Names
import Parser.Pattern
import Parser.Term
import Parser.Type
import Syntax.Module
import Syntax.Pattern
import Syntax.Type



-- RawModule RMod ::=  module Imp* where Decl*  (Raw Module)
parseRawModule :: AsteriaParser RawModule
parseRawModule =
  do imps <- many parseImport
     decls <- many parseDeclaration
     return $ RawModule imps decls



-- Import Imp ::=  import MName ImpAs? UH? Rens?;
parseImport :: AsteriaParser Import
parseImport = do
  try $ symbol "import"
  mp <- parseModulePath
  impAs <- parseImportAs
  usingHiding <- parseUsingHiding
  rens <- parseRenamings
  symbol ";"
  return $ Import mp impAs usingHiding rens



-- ImportAs ImpAs ::=  0         (No Import As)
--                  |  as MName  (Import As)
parseImportAs :: AsteriaParser ImportAs
parseImportAs =
  do mloc <- optional (symbol "as" >> parseModuleName)
     case mloc of
       Nothing -> return NoLocalName
       Just mn -> return (UseLocalName mn)



-- UsingHiding UH ::=  0             (No Using Hiding)
--                  |  using (DN+,)   (Using)
--                  |  hiding (DN+,)  (Hiding)
parseUsingHiding :: AsteriaParser UsingHiding
parseUsingHiding =
  do muh <- optional
              ((symbol "using" >> (UsingOnly <$> parens (sepBy1 parseDeclaredName comma)))
                <|> (symbol "hiding" >> (HidingOnly <$> parens (sepBy1 parseDeclaredName comma))))
     case muh of
       Nothing -> return UsingAll
       Just uh -> return uh



-- DeclaredName Dn
parseDeclaredName :: AsteriaParser DeclaredName
parseDeclaredName =
      DeclaredIdent <$> parseIdentName
  <|> DeclaredVar <$> parseVarName



-- Renamings Rens ::=  renaming (Ren+,)  (Renamings)
parseRenamings :: AsteriaParser [Renaming]
parseRenamings =
  do mrens <- optional $ do
                symbol "renaming"
                parens (sepBy1 parseRenaming comma)
     case mrens of
       Nothing -> return []
       Just rens -> return  rens



-- Renaming Ren ::=  Ident to Ident  (Rename Identifier)
--                |  Var to Var      (Rename Variable)
parseRenaming :: AsteriaParser Renaming
parseRenaming =
      parseRenameIdentifier
  <|> parseRenameVariable
  where

    parseRenameIdentifier :: AsteriaParser Renaming
    parseRenameIdentifier =
      do ident1 <- parseIdentName
         symbol "to"
         ident2 <- parseIdentName
         return $ RenameIdent ident1 ident2

    parseRenameVariable :: AsteriaParser Renaming
    parseRenameVariable =
      do var1 <- parseVarName
         symbol "to"
         var2 <- parseVarName
         return $ RenameVar var1 var2



-- Declaration Decl
--   ::=  data Cn (TVK)* where ConDecl* end             (Data Declaration)
--     |  type f (TVK)* = A;                            (Type Synonym Declaration)
--     |  class CCn (TVK)* <= (CC*) where MetDecl* end  (Type Class Declaration)
--     |  x : A;                                        (Term Type Signature)
--     |  x Pat* | M? = N;                              (Term Equation)
--     |  instance CCn A+ <= (CC*) where MetEq* end     (Instance Declaration)
parseDeclaration :: AsteriaParser Declaration
parseDeclaration =
      parseDataDeclaration
  <|> parseTypeSynonymDeclaration
  <|> parseTypeClassDeclaration
  <|> parseTermTypeSignatureDeclaration
  <|> parseTermEquationDeclaration
  <|> parseInstanceDeclaration
  where

    parseDataDeclaration :: AsteriaParser Declaration
    parseDataDeclaration = parseIndentDataDeclaration

    parseIndentDataDeclaration :: AsteriaParser Declaration
    parseIndentDataDeclaration = do
      try $ symbol "data"
      tn <- parseTypeName
      tvk <- many (parens parseTyVarKinding)
      symbol "where"
      decls <- many parseConstructorDeclaration
      symbol "end"
      return $ DataDecl tn tvk decls

    parseTypeSynonymDeclaration :: AsteriaParser Declaration
    parseTypeSynonymDeclaration = do
      try $ symbol "type"
      f <- parseTypeVar
      tvks <- many (parens parseTyVarKinding)
      symbol "="
      a <- parseType
      symbol ";"
      return $ TypeSynonymDecl f tvks a

    parseTypeClassDeclaration :: AsteriaParser Declaration
    parseTypeClassDeclaration = parseIndentTypeClassDeclaration

    parseIndentTypeClassDeclaration :: AsteriaParser Declaration
    parseIndentTypeClassDeclaration = do
      try $ symbol "class"
      cn <- parseClassName
      tvks <- many (parens parseTyVarKinding)
      cons <- optional $ do
                symbol "<="
                parens $ some parseClassConstraint
      symbol "where"
      decls <- many parseMethodDeclaration
      symbol "end"
      return $ ClassDecl cn tvks (maybe [] id cons) decls

    parseTermTypeSignatureDeclaration :: AsteriaParser Declaration
    parseTermTypeSignatureDeclaration = do
      x <- try $ do
        x <- parseTermVar
        symbol ":"
        return x
      a <- parseType
      symbol ";"
      return $ TermTypeSig x a

    parseTermEquationDeclaration :: AsteriaParser Declaration
    parseTermEquationDeclaration = do
      (x,pats) <- try $ do
        x <- parseTermVar
        pats <- many parseArgPattern
        symbol "="
        return (x,pats)
      n <- parseTerm
      symbol ";"
      return $ TermEquation x pats n

    parseInstanceDeclaration :: AsteriaParser Declaration
    parseInstanceDeclaration = parseIndentInstanceDeclaration

    parseIndentInstanceDeclaration :: AsteriaParser Declaration
    parseIndentInstanceDeclaration = do
      try $ symbol "instance"
      cn <- parseClassName <?> "instance class name"
      tvks <- many parseInstanceArgument
      cons <- optional $ do
                symbol "<="
                parens $ some parseClassConstraint
      symbol "where" <?> "where"
      decls <- many parseMethodEquation
      symbol "end"
      return $ InstanceDecl cn tvks (maybe [] id cons) decls

    parseInstanceArgument :: AsteriaParser Type
    parseInstanceArgument =
          VarT <$> parseTypeVar
      <|> NameT <$> parseTypeName
      <|> parens parseType



-- ConstructorDecl ConDecl ::=  Cn CP* : A;  (Constructor Declaration)
parseConstructorDeclaration :: AsteriaParser ConstructorDecl
parseConstructorDeclaration =
  do cn <- parseTermName
     vks <- many parseConstructorParameter
     symbol ":"
     a <- parseType
     symbol ";"
     return $ ConstructorDecl cn vks a



-- ConParam CP ::=  {TVK}   (Implicit Type Parameter)
--               |  (VT)    (Explicit Term Parameter)
parseConstructorParameter :: AsteriaParser ConstructorParameter
parseConstructorParameter =
      parseImplicitTypeParameter
  <|> parseExplicitTermParameter

parseImplicitTypeParameter :: AsteriaParser ConstructorParameter
parseImplicitTypeParameter = ImplicitTypeParameter <$> braces parseTyVarKinding

parseExplicitTermParameter :: AsteriaParser ConstructorParameter
parseExplicitTermParameter = ExplicitTermParameter <$> parens parseVarTyping


-- VarTyping VT ::=  x : A  (Variable Typing)
parseVarTyping :: AsteriaParser VarTyping
parseVarTyping =
  do x <- parseTermVar
     symbol ":"
     a <- parseType
     return $ VarTyping x a



-- MethodDeclaration MetDecl ::=  x : A;  (Method Declaration)
parseMethodDeclaration :: AsteriaParser MethodDeclaration
parseMethodDeclaration =
  do x <- parseTermVar
     symbol ":"
     a <- parseType
     symbol ";"
     return $ MethodDeclaration x a



-- MethodEquation MetEq ::=  x Pat* = N;  (Method Equation)
parseMethodEquation :: AsteriaParser MethodEquation
parseMethodEquation =
  do x <- parseTermVar
     pats <- many parseArgPattern
     symbol "="
     n <- parseTerm
     symbol ";"
     return $ MethodEquation x pats n
