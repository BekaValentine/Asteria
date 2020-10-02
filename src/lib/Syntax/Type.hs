module Syntax.Type where

import Syntax.Kind
import Syntax.Names



-- Type A, B ::=  a                 (Type Variable)
--             |  Cn                (Type Name)
--             |  A -> B            (Function Type)
--             |  CC => A           (Constrained Type)
--             |  forall {TVK}+. B  (Forall Type)
--             |  A B               (Type Application)
data Type = VarT TypeVar
          | NameT TypeName
          | FunT Type Type
          | ConstrainedT ClassConstraint Type
          | ForallT TyVarKinding Type
          | AppT Type Type
  deriving (Show,Eq)



-- ClassConstraint CC ::=  CCn a+  (Class Constraint)
data ClassConstraint = ClassConstraint ClassName [TypeVar]
  deriving (Show,Eq)


-- TyVarKinding TVK ::=  a : K  (Type Variable Kinding)
data TyVarKinding = TyVarKinding TypeVar Kind
  deriving (Show,Eq)
