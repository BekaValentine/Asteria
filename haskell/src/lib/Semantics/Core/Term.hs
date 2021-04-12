module Semantics.Core.Term where

import Syntax.Names
import Semantics.Core.Names





-- Term M, N ::=  x                (Variable)
--             |  dx               (Declared Variable)
--             |  \x -> M          (Lambda Abstraction)
--             |  M N              (Function Application)
--             |  Cn M*            (Constructed Data)
--             |  case M+ of Cls*   (Case)
--             |  let LDecl+ in M  (Let)
data Term = Var TermVar
          | DecVar DeclaredTermVar
          | Lam TermVar Term
          | App Term Term
          | Con DeclaredTermName [Term]
          | Case [Term] [CaseClause]
          | Let [LocalDecl] Term
  deriving (Show,Eq)



-- CaseClause Cls ::=  Pat+ -> M  (Case Clause)
data CaseClause = CaseClause [Pattern] Term
  deriving (Show,Eq)



-- LocalDec LDecl ::=  x = M  (Let DeclarationTerm Equation)
data LocalDecl = LocalTermEquation TermVar Term
  deriving (Show,Eq)



-- Pattern Pat ::=  x        (Variable Pattern)
--               |  Cn Pat*  (Constructor Pattern)
data Pattern = VarPat TermVar
             | ConPat TermName [Pattern]
  deriving (Show,Eq)
