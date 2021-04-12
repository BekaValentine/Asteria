module Parser.Module where

import Text.Megaparsec
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.Combinators

import Parser.Common
import Parser.Names
import Parser.Pattern
import Parser.Term
import Parser.Type
import Syntax.Module
import Syntax.Pattern
import Syntax.Type



-- RawModule RMod ::=  module TVK* Imp* where Decl*  (Raw Module)
parseRawModule :: AsteriaParser RawModule
parseRawModule =
  do symbol "module"
     newlineSpaceConsumer
     imps <- many parseImport
     newlineSpaceConsumer
     symbol "where"
     decls <- many parseDeclaration
     return $ RawModule imps decls



-- Import Imp ::=  import MName ImpAs? UH? Rens?
parseImport :: AsteriaParser Import
parseImport = try $ L.nonIndented newlineSpaceConsumer $ L.indentBlock newlineSpaceConsumer p
  where
    p = do
      try $ symbol "import"
      mp <- parseModulePath
      impAs <- parseImportAs
      usingHiding <- parseUsingHiding
      rens <- parseRenamings
      return $ L.IndentNone (Import mp impAs usingHiding rens)



-- ImportAs ImpAs ::=  0         (No Import As)
--                  |  as MName  (Import As)
parseImportAs :: AsteriaParser ImportAs
parseImportAs =
  do mloc <- optional (symbol "as" >> parseModuleName)
     case mloc of
       Nothing -> return NoLocalName
       Just mn -> return (UseLocalName mn)



-- UsingHiding UH ::=  0             (No Using Hiding)
--                  |  using (DN*)   (Using)
--                  |  hiding (DN*)  (Hiding)
parseUsingHiding :: AsteriaParser UsingHiding
parseUsingHiding =
  do muh <- optional
              ((symbol "using" >> (UsingOnly <$> parens (some parseDeclaredName)))
                <|> (symbol "hiding" >> (HidingOnly <$> parens (some parseDeclaredName))))
     case muh of
       Nothing -> return UsingAll
       Just uh -> return uh



-- DeclaredName Dn
parseDeclaredName :: AsteriaParser DeclaredName
parseDeclaredName =
      DeclaredIdent <$> parseIdentName
  <|> DeclaredVar <$> parseVarName



-- Renamings Rens ::=  renaming (Ren+)  (Renamings)
parseRenamings :: AsteriaParser [Renaming]
parseRenamings =
  do mrens <- optional $ do
                symbol "renaming"
                parens (sepBy parseRenaming (symbol ";"))
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
--   ::=  data Cn TVK* where ConDecl*             (Data Declaration)
--     |  type f TVK* = A                         (Type Synonym Declaration)
--     |  class CCn TVK* <= (CC*) where MetDecl*  (Type Class Declaration)
--     |  x : A                                   (Term Type Signature)
--     |  x Pat* | M? = N                         (Term Equation)
--     |  instance CCn A+ <= (CC*) where MetEq*   (Instance Declaration)
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
    parseIndentDataDeclaration = try $ L.nonIndented newlineSpaceConsumer $ L.indentBlock newlineSpaceConsumer p
      where
        p = do try $ symbol "data"
               tn <- parseTypeName
               tvk <- many parseTyVarKinding
               symbol "where"
               return (L.IndentMany
                        Nothing
                        (return . DataDecl tn tvk)
                        parseConstructorDeclaration)

    parseTypeSynonymDeclaration :: AsteriaParser Declaration
    parseTypeSynonymDeclaration = try $ L.nonIndented newlineSpaceConsumer $ L.indentBlock newlineSpaceConsumer p
      where
        p = do try $ symbol "type"
               f <- parseTypeVar
               tvks <- many parseTyVarKinding
               symbol "="
               a <- parseType
               return (L.IndentNone (TypeSynonymDecl f tvks a))

    parseTypeClassDeclaration :: AsteriaParser Declaration
    parseTypeClassDeclaration = parseIndentTypeClassDeclaration

    parseIndentTypeClassDeclaration :: AsteriaParser Declaration
    parseIndentTypeClassDeclaration = try $ L.nonIndented newlineSpaceConsumer $ L.indentBlock newlineSpaceConsumer p
      where
        p = do try $ symbol "class"
               cn <- parseClassName
               tvks <- many parseTyVarKinding
               cons <- optional $ do
                         symbol "<="
                         parens $ some parseClassConstraint
               symbol "where"
               return (L.IndentMany
                        Nothing
                        (return . ClassDecl cn tvks (maybe [] id cons))
                        parseMethodDeclaration)

    parseTermTypeSignatureDeclaration :: AsteriaParser Declaration
    parseTermTypeSignatureDeclaration = try $ L.nonIndented newlineSpaceConsumer $ L.indentBlock newlineSpaceConsumer p
      where
        p = do x <- try $ do
                 x <- parseTermVar
                 symbol ":"
                 return x
               a <- parseType
               return (L.IndentNone (TermTypeSig x a))

    parseTermEquationDeclaration :: AsteriaParser Declaration
    parseTermEquationDeclaration = try $ L.nonIndented newlineSpaceConsumer $ L.indentBlock newlineSpaceConsumer p
      where
        p = do (x,pats) <- try $ do
                 x <- parseTermVar
                 pats <- many parseEquationArg
                 symbol "="
                 return (x,pats)
               n <- parseTerm
               return (L.IndentNone (TermEquation x pats n))

    parseEquationArg :: AsteriaParser Pattern
    parseEquationArg =
          parseVariablePattern
      <|> parseConstructorPatternNoArguments
      <|> parens parsePattern

    parseInstanceDeclaration :: AsteriaParser Declaration
    parseInstanceDeclaration = parseIndentInstanceDeclaration

    parseIndentInstanceDeclaration :: AsteriaParser Declaration
    parseIndentInstanceDeclaration = try $ L.nonIndented newlineSpaceConsumer $ L.indentBlock newlineSpaceConsumer p
      where
        p = do try $ symbol "instance"
               cn <- parseClassName <?> "instance class name"
               tvks <- many parseInstanceArgument
               cons <- optional $ do
                         symbol "<="
                         parens $ some parseClassConstraint
               symbol "where" <?> "where"
               return (L.IndentMany
                        Nothing
                        (return . InstanceDecl cn tvks (maybe [] id cons))
                        parseMethodEquation)

    parseInstanceArgument :: AsteriaParser Type
    parseInstanceArgument =
          VarT <$> parseTypeVar
      <|> NameT <$> parseTypeName
      <|> parens parseType



-- ConstructorDecl ConDecl ::=  Cn VK* : A  (Constructor Declaration)
parseConstructorDeclaration :: AsteriaParser ConstructorDecl
parseConstructorDeclaration =
  do cn <- parseTermName
     vks <- many (parens parseVarTyping)
     symbol ":"
     a <- parseType
     return $ ConstructorDecl cn vks a



-- VarTyping VT ::=  x : A  (Variable Typing)
parseVarTyping :: AsteriaParser VarTyping
parseVarTyping =
  do x <- parseTermVar
     symbol ":"
     a <- parseType
     return $ VarTyping x a



-- MethodDeclaration MetDecl ::=  x : A  (Method Declaration)
parseMethodDeclaration :: AsteriaParser MethodDeclaration
parseMethodDeclaration =
  do x <- parseTermVar
     symbol ":"
     a <- parseType
     return $ MethodDeclaration x a



-- MethodEquation MetEq ::=  x Pat* = N  (Method Equation)
parseMethodEquation :: AsteriaParser MethodEquation
parseMethodEquation =
  do x <- parseTermVar
     pats <- many parseEquationArg
     symbol "="
     n <- parseTerm
     return $ MethodEquation x pats n
  where
    parseEquationArg :: AsteriaParser Pattern
    parseEquationArg =
          parseVariablePattern
      <|> parseConstructorPatternNoArguments
      <|> parens parsePattern
