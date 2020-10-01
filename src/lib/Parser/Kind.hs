module Parser.Kind where

import Control.Applicative
import Control.Monad.Combinators.Expr
import Text.Megaparsec

import Parser.Common
import Syntax.Kind



-- Kind K, J ::=  Type    (Type Kind)
--             |  K -> J  (Function Kind)
parseKind :: AsteriaParser Kind
parseKind = parseKindExpr
  where

    parseKindExpr = makeExprParser parseKindTerm kindOperators <?> "kind"

    parseKindTerm = parens parseKindExpr <|> parseTypeKind

    kindOperators = [[ InfixL  (FunK <$ symbol "->") ]]

    parseTypeKind :: AsteriaParser Kind
    parseTypeKind =
      do symbol "Type"
         return $ TypeK
