module Syntax.Term  where

import Syntax.Names
import Syntax.Pattern
import Syntax.Type




-- Term M, N ::=  M : A           (Type Annotation)
--             |  x               (Variable)
--             | \x+ -> M         (Lambda Abstraction)
--             | M N              (Function Application)
--             | M @ A            (Type Instantiation)
--             | case M of Cls*   (Case)
--             | let LDecl* in M  (Let)
data Term = Ann Term Type
          | Var TermVar
          | Lam [TermVar] Term
          | App Term Term
          | AppTy Term Type
          | Case Term [CaseClause] Term
          | Let [LetDecl] Term



-- CaseClause Cls ::=  Pat | M* -> N  (Case Clause)
data CaseClause = CaseClause Pattern [Term] Term



-- LetDec LDecl ::=  x : A       (Let DeclarationTerm Term Type Signature)
--                |  x Pat* = M  (Let DeclarationTerm Equation)
data LetDecl = LetTermTypeSig TermVar Type
             | LetTermEquation TermVar [Pattern] [Term] Term
