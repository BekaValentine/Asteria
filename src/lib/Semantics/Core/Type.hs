module Semantics.Core.Type where

import Semantics.Core.Names
import Syntax.Kind
import Syntax.Names



-- Type A, B ::=  m              (Metavariable)
--             |  a              (Type Variable)
--             |  da             (Declared Type Variable)
--             |  cn A*          (Constructed Type)
--             |  A -> B         (Function Type)
--             |  CC => A        (Constrained Type)
--             |  forall TVK. B  (Forall Type)
--             |  \TVK -> B      (Type Function)
--             |  A B            (Type Application)
data Type = MetaVarT MetaVar
          | VarT TypeVar
          | DecVarT DeclaredTypeVar
          | ConT DeclaredTypeName [Type]
          | FunT Type Type
          | ConstrainedT ClassConstraint Type
          | ForallT TyVarKinding Type
          | LamT TyVarKinding Type
          | AppT Type Type
  deriving (Show,Eq)



-- ClassConstraint CC ::=  CCn a+  (Class Constraint)
data ClassConstraint = ClassConstraint DeclaredClassName [TypeVar]
  deriving (Show,Eq)


-- TyVarKinding TVK ::=  (a : K)  (Type Variable Kinding)
data TyVarKinding = TyVarKinding TypeVar Kind
  deriving (Show,Eq)
