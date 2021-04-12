module Semantics.Static.Term where

import Semantics.Static.Common
import qualified Semantics.Core.Term as Core
import Semantics.Static.Type
import Syntax.Kind
import Syntax.Names
import Syntax.Term
import Syntax.Type





{-
-- Term M, N ::=  M : A               (Type Annotation)
--             |  x                   (Variable)
--             |  Cn                  (Constructor)
--             |  \x -> M             (Lambda Abstraction)
--             |  M N                 (Function Application)
--             |  M @ A               (Type Instantiation)
--             |  case M+ of Cls*     (Case)
--             |  let LDecl+ in M     (Let)
--             |  define LDecl+ in M  (Define)
-}

--  [ G !- A ni M >> M' ]
--
--  G, a : K !- B ni M >> M'
--  ---------------------------------
--  G !- forall (a : K). B ni M >> M'
--
--  G, x : A !- B ni M >> M'
--  ----------------------------------
--  G !- A -> B ni \x -> M >> \x -> M'
--
--  G !- M_i >> M'_i in A_i    G !- B ni Cls_i >> Cls'_i clause A*    Cls* covers A*
--  --------------------------------------------------------------------------------
--  G !- B ni case M+ of Cls* >> case M'+ of Cls'*
--
--  G, CTX(LDecl'_{0..i-1}) !- LDecl_i >> LDecl'_i letdecl    G, CTX(LDecl'+) !- B ni M >> M'
--  -----------------------------------------------------------------------------------------
--  G !- B ni let LDecl+ in M >> let LDecl'+ in M'
--
--  G, CTX(LDecl'_{0..i-1}) !- LDecl_i >> LDecl'_i defdecl    G, CTX(LDecl'+) !- B ni M >> M'
--  -----------------------------------------------------------------------------------------
--  G !- B ni define LDecl+ in M >> M'
--
--  G !- M >> M' in A'    A <: A'
--  -----------------------------
--  G !- A ni M >> M'
checkTerm :: Context -> Type -> Term -> Elaborator Core.Term

checkTerm g (ForallT tvks b) m =
  _

checkTerm g (FunT a b) (Lam x m) =
  checkTerm ((x,a) : g) b m

checkTerm g b (Case ms cls) =
  do mas' <- mapM (synthTerm g) ms
     let (ms', as) = unzip mas'
     cls' <- mapM (checkClause g b as) cls
     return $ Core.Case ms' cls'

checkTerm g b (Let decls m) =
  do (g', decls') <- synthLetDecls g decls
     checkTerm g' b m

checkTerm g b (Def decls m) =
  _

checkTerm g a m =
  do (m',a') <- synthTerm g m
     subtype a' a
     return m'

--  [ G !- M >> M' in A ]
--
--  G !- A ni M >> M'
--  ---------------------
--  G !- M : A >> M' in A
--
--  G ni x : A
--  ----------------
--  G !- x >> x in A
--
--  D ni Cn := M' : A
--  ------------------
--  G !- Cn >> M' in A
--
--  G !- M >> M' in A -> B    G !- A ni N >> N'
--  -------------------------------------------
--  G !- M N >> M' N' in B
--
--  G !- M >> M' in forall (a : K). B    G !- K ni A
--  ------------------------------------------------
--  G !- M @ A >> M' in [A/a]B
--
--  G !- M_i >> M'_i in A_i    G !- Cls_i >> Cls'_i clause A* in B    Cls* covers A*
--  --------------------------------------------------------------------------------
--  G !- case M+ of Cls* >> case M'+ of Cls'* in B
--
--  G, CTX(LDecl'_{0..i-1}) !- LDecl_i >> LDecl'_i letdecl    G, CTX(LDecl'+) !- M >> M' in B
--  -----------------------------------------------------------------------------------------
--  G !- let LDecl+ in M >> let LDecl'+ in M' in B
--
--  G, CTX(LDecl'_{0..i-1}) !- LDecl_i >> LDecl'_i defdecl    G, CTX(LDecl'+) !- B ni M >> M'
--  -----------------------------------------------------------------------------------------
--  G !- B ni define LDecl+ in M >> M'
synthTerm :: Context -> Term -> Elaborator (Core.Term, Type)
synthTerm g m = undefined



checkClause :: Context -> Type -> [Type] -> CaseClause -> Elaborator Core.CaseClause
checkClause = _



synthLetDecls :: Context -> [LocalDecl] -> Elaborator (Context, [LocalDecl])
synthLetDecls g [] =
  return (g, [])
synthLetDecls g (d:ds) =
  do (g', md') <- synthLetDecl g d
     (g'', ds') <- synthLetDecls g' ds
     case md' of
       Nothing -> return (g'', ds')
       Just d' -> return (g'', d':ds')



synthLetDecl :: Context -> LocalDecl -> Elaborator (Context, Maybe LocalDecl)
synthLetDecl g (LocalTermTypeSig x a) =
  do checkType g TypeK a
     return ((x,a):g, Nothing)
synthLetDecl g (LocalTermEquation x ps m) =
  _



subtype :: Type -> Type -> Elaborator ()
subtype a b = _
