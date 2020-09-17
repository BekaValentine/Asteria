module Parser.Module where

import Text.Parsec

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
  do reserved "module"
     tvks <- many parseTyVarKinding
     imps <- many parseImport
     reserved "where"
     decls <- many parseDeclaration
     return $ RawModule tvks imps decls



-- Import Imp ::=  import MName ImpAs? UH? Rens?
parseImport :: AsteriaParser Import
parseImport =
  do reserved "import"
     mp <- parseModulePath
     impAs <- parseImportAs
     usingHiding <- parseUsingHiding
     rens <- parseRenamings
     return $ Import mp impAs usingHiding rens



-- ImportAs ImpAs ::=  0         (No Import As)
--                  |  as MName  (Import As)
parseImportAs :: AsteriaParser ImportAs
parseImportAs =
  do mloc <- optionMaybe (reserved "as" >> parseModuleName)
     case mloc of
       Nothing -> return NoLocalName
       Just mn -> return (UseLocalName mn)



-- UsingHiding UH ::=  0             (No Using Hiding)
--                  |  using (DN*)   (Using)
--                  |  hiding (DN*)  (Hiding)
parseUsingHiding :: AsteriaParser UsingHiding
parseUsingHiding =
  do muh <- optionMaybe
              ((reserved "using" >> (UsingOnly <$> parens (many1 parseDeclaredName)))
                <|> (reserved "hiding" >> (HidingOnly <$> parens (many1 parseDeclaredName))))
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
  do mrens <- optionMaybe $ do
                reserved "renaming"
                parens (sepBy parseRenaming (reservedOp ";"))
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
         reserved "to"
         ident2 <- parseIdentName
         return $ RenameIdent ident1 ident2

    parseRenameVariable :: AsteriaParser Renaming
    parseRenameVariable =
      do var1 <- parseVarName
         reserved "to"
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
    parseDataDeclaration =
      do reserved "data"
         tn <- parseTypeName
         tvk <- many parseTyVarKinding
         reserved "where"
         condecls <- braces (sepBy parseConstructorDeclaration (reservedOp "|"))
         return $ DataDecl tn tvk condecls

    parseTypeSynonymDeclaration :: AsteriaParser Declaration
    parseTypeSynonymDeclaration =
      do reserved "type"
         f <- parseTypeVar
         tvks <- many parseTyVarKinding
         reservedOp "="
         a <- parseType
         reservedOp ";"
         return $ TypeSynonymDecl f tvks a

    parseTypeClassDeclaration :: AsteriaParser Declaration
    parseTypeClassDeclaration =
      do reserved "class"
         cn <- parseClassName
         tvks <- many parseTyVarKinding
         cons <- optionMaybe $ do
                   reservedOp "<="
                   parens $ many1 parseClassConstraint
         reserved "where"
         cls <- braces (sepBy parseMethodDeclaration (reservedOp ";"))
         return $ ClassDecl cn tvks (maybe [] id cons) cls

    parseTermTypeSignatureDeclaration :: AsteriaParser Declaration
    parseTermTypeSignatureDeclaration =
      do x <- try $ do
           x <- parseTermVar
           reservedOp ":"
           return x
         a <- parseType
         reservedOp ";"
         return $ TermTypeSig x a

    parseTermEquationDeclaration :: AsteriaParser Declaration
    parseTermEquationDeclaration =
      do (x,pats) <- try $ do
           x <- parseTermVar
           pats <- many parseEquationArg
           reservedOp "="
           return (x,pats)
         n <- parseTerm
         reservedOp ";"
         return $ TermEquation x pats n

    parseEquationArg :: AsteriaParser Pattern
    parseEquationArg =
          parseVariablePattern
      <|> parseConstructorPatternNoArguments
      <|> parens parsePattern

    parseInstanceDeclaration :: AsteriaParser Declaration
    parseInstanceDeclaration =
      do reserved "instance"
         cn <- parseClassName
         tvks <- many parseInstanceArgument
         cons <- optionMaybe $ do
                   reservedOp "<="
                   parens $ many1 parseClassConstraint
         reserved "where"
         cls <- braces (sepBy parseMethodEquation (reservedOp ";"))
         return $ InstanceDecl cn tvks (maybe [] id cons) cls

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
     reservedOp ":"
     a <- parseType
     return $ ConstructorDecl cn vks a



-- VarTyping VT ::=  x : A  (Variable Typing)
parseVarTyping :: AsteriaParser VarTyping
parseVarTyping =
  do x <- parseTermVar
     reservedOp ":"
     a <- parseType
     return $ VarTyping x a



-- MethodDeclaration MetDecl ::=  x : A  (Method Declaration)
parseMethodDeclaration :: AsteriaParser MethodDeclaration
parseMethodDeclaration =
  do x <- parseTermVar
     reservedOp ":"
     a <- parseType
     return $ MethodDeclaration x a



-- MethodEquation MetEq ::=  x Pat* = N  (Method Equation)
parseMethodEquation :: AsteriaParser MethodEquation
parseMethodEquation =
  do x <- parseTermVar
     pats <- many parseEquationArg
     reservedOp "="
     n <- parseTerm
     return $ MethodEquation x pats n
  where
    parseEquationArg :: AsteriaParser Pattern
    parseEquationArg =
          parseVariablePattern
      <|> parseConstructorPatternNoArguments
      <|> parens parsePattern
{-
-- MethodEquation MetEq ::=  x Pat* | M? = N  (Method Equation)
data MethodEquation = MethodEquation TermVar [Pattern] (Maybe Term) Term
  deriving (Show,Eq)
-}
