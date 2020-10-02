module Parser.Type where

import Text.Megaparsec

import Parser.Common
import Parser.Kind
import Parser.Names
import Syntax.Type



-- Type A, B ::=  a                 (Type Variable)
--             |  Cn                (Type Name)
--             |  A -> B            (Function Type)
--             |  CC => A           (Constrained Type)
--             |  forall {TVK}+. B  (Forall Type)
--             |  A B               (Type Application)
parseType :: AsteriaParser Type
parseType = parseTypeExpr
  where

    parseTypeExpr = parseFunctionType <|> parseNonFunctionType

    parseFunctionType :: AsteriaParser Type
    parseFunctionType =
      do a <- try $ do
           a <- parseNonFunctionType
           symbol "->"
           return a
         b <- parseTypeExpr
         return (FunT a b)

    parseNonFunctionType :: AsteriaParser Type
    parseNonFunctionType =
          parseConstrainedType
      <|> parseForallType
      <|> parseApplicationType

    parseConstrainedType :: AsteriaParser Type
    parseConstrainedType =
      do ccs <- try $ do
           ccs <- parens (sepBy parseClassConstraint (symbol ","))
           symbol "=>"
           return ccs
         b <- parseType
         return $ foldr ConstrainedT b ccs

    parseForallType :: AsteriaParser Type
    parseForallType =
      do try $ symbol "forall"
         as <- some (braces parseTyVarKinding)
         symbol "."
         b <- parseType
         return $ foldr ForallT b as

    parseApplicationType :: AsteriaParser Type
    parseApplicationType =
      do t:ts <- some parseNonApplicationType
         return (foldl AppT t ts)

    parseNonApplicationType :: AsteriaParser Type
    parseNonApplicationType =
          (parseVarType <?> "var type")
      <|> (parseNameType <?> "con type")
      <|> (parens parseTypeExpr <?> "parenthesized type")

    parseVarType :: AsteriaParser Type
    parseVarType = VarT <$> parseTypeVar

    parseNameType :: AsteriaParser Type
    parseNameType = NameT <$> parseTypeName



-- ClassConstraint CC ::=  CCn a+  (Class Constraint)
parseClassConstraint :: AsteriaParser ClassConstraint
parseClassConstraint =
  ClassConstraint <$> (parseClassName <?> "class constraint name") <*> some (parseTypeVar <?> "class constraint argument")



-- TyVarKinding TVK ::=  a : K  (Type Variable Kinding)
parseTyVarKinding :: AsteriaParser TyVarKinding
parseTyVarKinding = do
  a <- parseTypeVar
  symbol ":"
  k <- parseKind
  return $ TyVarKinding a k
