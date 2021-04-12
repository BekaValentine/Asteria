module Syntax.Term  where

import Syntax.Names
import Syntax.Pattern
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
data Term = Ann Term Type
          | Var TermVar
          | Name TermName
          | Lam TermVar Term
          | App Term Term
          | Abs TypeVar Term
          | Inst Term Type
          | Case [Term] [CaseClause]
          | Let [LocalDecl] Term
  deriving (Show,Eq)



-- CaseClause Cls ::=  Pat -> N  (Case Clause)
data CaseClause = CaseClause [Pattern] Term
  deriving (Show,Eq)



-- LocalDec LDecl ::=  x : A       (Let DeclarationTerm Term Type Signature)
--                  |  x Pat* = M  (Let DeclarationTerm Equation)
data LocalDecl = LocalTermTypeSig TermVar Type
               | LocalTermEquation TermVar [Pattern] Term
  deriving (Show,Eq)
