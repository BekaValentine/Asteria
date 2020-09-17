module Parser.Type where

import Text.Parsec

import Parser.Common
import Parser.Kind
import Parser.Names
import Syntax.Type



-- Type A, B ::=  a              (Type Variable)
--             |  Cn             (Type Name)
--             |  A -> B         (Function Type)
--             |  CC => A        (Constrained Type)
--             |  forall TVK. B  (Forall Type)
--             |  A B            (Type Application)
parseType :: AsteriaParser Type
parseType = parseFunctionType
  where

    parseFunctionType :: AsteriaParser Type
    parseFunctionType =
      chainr1 parseNonFunctionType (reservedOp "->" >> return FunT)

    parseNonFunctionType :: AsteriaParser Type
    parseNonFunctionType =
          parseConstrainedType
      <|> parseForallType
      <|> parseApplicationType

    parseConstrainedType :: AsteriaParser Type
    parseConstrainedType =
      do ccs <- parens (sepBy parseClassConstraint (reservedOp ","))
         reservedOp "=>"
         b <- parseType
         return $ foldr ConstrainedT b ccs

    parseForallType :: AsteriaParser Type
    parseForallType =
      do try $ reserved "forall"
         as <- many1 parseTyVarKinding
         reservedOp "."
         b <- parseType
         return $ foldr ForallT b as

    parseApplicationType :: AsteriaParser Type
    parseApplicationType =
      chainl1 parseNonApplicationType (return AppT)

    parseNonApplicationType :: AsteriaParser Type
    parseNonApplicationType =
          parseVarType
      <|> parseNameType

    parseVarType :: AsteriaParser Type
    parseVarType = VarT <$> parseTypeVar

    parseNameType :: AsteriaParser Type
    parseNameType = NameT <$> parseTypeName



-- ClassConstraint CC ::=  CCn a+  (Class Constraint)
parseClassConstraint :: AsteriaParser ClassConstraint
parseClassConstraint =
  ClassConstraint <$> parseClassName <*> many1 parseTypeVar



-- TyVarKinding TVK ::=  (a : K)  (Type Variable Kinding)
parseTyVarKinding :: AsteriaParser TyVarKinding
parseTyVarKinding =
  parens $ do
    a <- parseTypeVar
    reservedOp ":"
    k <- parseKind
    return $ TyVarKinding a k
