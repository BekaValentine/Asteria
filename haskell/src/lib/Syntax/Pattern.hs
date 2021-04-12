module Syntax.Pattern where

import Syntax.Names



-- Pattern Pat ::=  x         (Variable Pattern)
--               |  Cn APat*  (Constructor Pattern)
data Pattern = VarPat TermVar
             | ConPat TermName [ArgPattern]
  deriving (Show,Eq)

-- ArgPattern APat ::=  Pat     (Normal Pattern Argument)
--                   | {TyVar}  (Type Instantiation Argument)
data ArgPattern = NormalPat Pattern
                | InstPat TypeVar
  deriving (Show,Eq)
