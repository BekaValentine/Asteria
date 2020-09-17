module Parser.Kind where

import Control.Applicative
import Text.Parsec.Combinator

import Parser.Common
import Syntax.Kind



-- Kind K, J ::=  Type    (Type Kind)
--             |  K -> J  (Function Kind)
parseKind :: AsteriaParser Kind
parseKind = parseFunctionKind
  where

    parseFunctionKind :: AsteriaParser Kind
    parseFunctionKind =
      chainr1 parseNonFunctionKind (reservedOp "->" >> pure FunK)

    parseNonFunctionKind :: AsteriaParser Kind
    parseNonFunctionKind =
      parseTypeKind <|> parens parseKind

    parseTypeKind :: AsteriaParser Kind
    parseTypeKind =
      do symbol "Type"
         return $ TypeK
