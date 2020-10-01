module Parser.Term where

import Data.List
import Text.Megaparsec

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
parseTerm :: AsteriaParser Term
parseTerm =
      parseSuffixed
  <|> parseLambda
  <|> parseCase
  <|> parseLet
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
      do try $ symbol ":"
         a <- parseType
         return $ \x -> Ann x a

    parseLambda :: AsteriaParser Term
    parseLambda =
      do try $ symbol "\\"
         xs <- some parseLambdaParam
         symbol "->"
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
      do try $ symbol "case"
         m <- sepBy1 parseTerm (symbol ",")
         symbol "of"
         cls <- parseCaseClauses
         return $ Case m cls

    parseCaseClauses :: AsteriaParser [CaseClause]
    parseCaseClauses = braceBlock ";" parseCaseClause

    parseLet :: AsteriaParser Term
    parseLet =
      do try $ symbol "let"
         decls <- parseLocalDecls
         symbol "in"
         m <- parseTerm
         return $ Let decls m

    parseLocalDecls :: AsteriaParser [LocalDecl]
    parseLocalDecls = braceBlock ";" parseLocalDecl



-- CaseClause Cls ::=  Pat | M* -> N  (Case Clause)
parseCaseClause :: AsteriaParser CaseClause
parseCaseClause =
  do pat <- sepBy1 parsePattern (symbol ",")
     symbol "->"
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
      do x <- try $ parseTermVar <* symbol ":"
         a <- parseType
         return $ LocalTermTypeSig x a

    parseEquation :: AsteriaParser LocalDecl
    parseEquation =
      do (x,pats) <- try $ do
                       x <- parseTermVar
                       pats <- many parseEquationArg
                       symbol "="
                       return (x,pats)
         n <- parseTerm
         return $ LocalTermEquation x pats n

    parseEquationArg :: AsteriaParser Pattern
    parseEquationArg =
          parseVariablePattern
      <|> parseConstructorPatternNoArguments
      <|> parens parsePattern
