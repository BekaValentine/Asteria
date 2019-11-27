module Syntax.Kind where



-- Kind K, J ::=  Type    (Type Kind)
--             |  K -> J  (Function Kind)

data Kind = TypeK
          | FunK Kind Kind
