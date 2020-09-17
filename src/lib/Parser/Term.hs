module Parser.Term where

import Data.List
import Text.Parsec
import Text.Parsec.Indent

import Parser.Common
import Parser.Names
import Parser.Pattern
import Parser.Type
import Syntax.Names
import Syntax.Pattern
import Syntax.Term
import Syntax.Type



-- Term M, N ::=  M : A               (Type Annotation)
--             |  x                   (Variable)
--             |  Cn                  (Constructor)
--             |  \x -> M             (Lambda Abstraction)
--             |  M N                 (Function Application)
--             |  \{a} -> M           (Type Abstraction)
--             |  M {A}               (Type Instantiation)
--             |  case M* of Cls*     (Case)
--             |  let LDecl+ in M     (Let)
--             |  define LDecl+ in M  (Define)
parseTerm :: AsteriaParser Term
parseTerm =
      parseSuffixed
  <|> parseLambda
  <|> parseCase
  <|> parseLet
  <|> parseDefine
  <|> parens parseTerm
  where

    parseSuffixed :: AsteriaParser Term
    parseSuffixed =
      do pre <- parsePrefix
         appSuff <- parseApplicationSuffix
         annSuff <- parseAnnotationSuffix
         return $ annSuff (appSuff pre)

    parsePrefix :: AsteriaParser Term
    parsePrefix =
          parseVariable
      <|> parseName

    parseVariable :: AsteriaParser Term
    parseVariable = Var <$> parseTermVar

    parseName :: AsteriaParser Term
    parseName = Name <$> parseTermName

    parseApplicationSuffix :: AsteriaParser (Term -> Term)
    parseApplicationSuffix =
      do xs <- many parseApplicationArgument
         return $ \f -> foldl' apply f xs

    apply :: Term -> Either Type Term -> Term
    apply f (Left a) = Inst f a
    apply f (Right m) = App f m

    parseApplicationArgument :: AsteriaParser (Either Type Term)
    parseApplicationArgument =
          Right <$> parseVariable
      <|> Right <$> parseName
      <|> Right <$> parens parseTerm
      <|> Left <$> braces parseType

    parseAnnotationSuffix :: AsteriaParser (Term -> Term)
    parseAnnotationSuffix =
      do suffs <- many parseAnnotation
         return $ \m -> foldl' (flip ($)) m suffs

    parseAnnotation :: AsteriaParser (Term -> Term)
    parseAnnotation =
      do try $ reservedOp ":"
         a <- parseType
         return $ \x -> Ann x a

    parseLambda :: AsteriaParser Term
    parseLambda =
      do try $ reservedOp "\\"
         xs <- many1 parseLambdaParam
         reservedOp "->"
         m <- parseTerm
         return $ foldr abstract m xs

    abstract :: Either TypeVar TermVar -> Term -> Term
    abstract (Right x) m = Lam x m
    abstract (Left x) m = Abs x m

    parseLambdaParam :: AsteriaParser (Either TypeVar TermVar)
    parseLambdaParam =
          Right <$> parseTermVar
      <|> Left <$> parseTypeVar

    parseCase :: AsteriaParser Term
    parseCase =
      do try $ reserved "case"
         m <- sepBy1 parseTerm (reservedOp ",")
         reserved "of"
         cls <- parseCaseClauses
         return $ Case m cls

    parseCaseClauses :: AsteriaParser [CaseClause]
    parseCaseClauses =
          parseDelimitedCaseClauses
      -- <|> parseIndentedCaseClauses

    parseDelimitedCaseClauses :: AsteriaParser [CaseClause]
    parseDelimitedCaseClauses =
      braces (sepBy parseCaseClause (reservedOp ";"))

    parseIndentedCaseClauses :: AsteriaParser [CaseClause]
    parseIndentedCaseClauses = block parseCaseClause

    parseLet :: AsteriaParser Term
    parseLet =
      do try $ reserved "let"
         decls <- parseLocalDecls
         reserved "in"
         m <- parseTerm
         return $ Let decls m

    parseDefine :: AsteriaParser Term
    parseDefine =
      do try $ reserved "define"
         decls <- parseLocalDecls
         reserved "in"
         m <- parseTerm
         return $ Def decls m

    parseLocalDecls :: AsteriaParser [LocalDecl]
    parseLocalDecls =
          parseDelimitedLocalDecls
      -- <|> parseIndentedLetDecls

    parseDelimitedLocalDecls :: AsteriaParser [LocalDecl]
    parseDelimitedLocalDecls =
      braces (sepBy1 parseLocalDecl (reservedOp ";"))

    parseIndentedLocalDecls :: AsteriaParser [LocalDecl]
    parseIndentedLocalDecls = block parseLocalDecl



-- CaseClause Cls ::=  Pat | M* -> N  (Case Clause)
parseCaseClause :: AsteriaParser CaseClause
parseCaseClause =
  do pat <- sepBy1 parsePattern (reservedOp ",")
     reservedOp "->"
     n <- parseTerm
     return $ CaseClause pat n



-- LocalDec LDecl ::=  x : A             (Local Type Signature)
--                  |  x Pat* `|` M = N  (Local Term Equation)
parseLocalDecl :: AsteriaParser LocalDecl
parseLocalDecl =
      parseTypeSig
  <|> parseEquation

  where

    parseTypeSig :: AsteriaParser LocalDecl
    parseTypeSig =
      do x <- try $ parseTermVar <* reservedOp ":"
         a <- parseType
         return $ LocalTermTypeSig x a

    parseEquation :: AsteriaParser LocalDecl
    parseEquation =
      do (x,pats) <- try $ do
                       x <- parseTermVar
                       pats <- many parseEquationArg
                       reservedOp "="
                       return (x,pats)
         n <- parseTerm
         return $ LocalTermEquation x pats n

    parseEquationArg :: AsteriaParser Pattern
    parseEquationArg =
          parseVariablePattern
      <|> parseConstructorPatternNoArguments
      <|> parens parsePattern
