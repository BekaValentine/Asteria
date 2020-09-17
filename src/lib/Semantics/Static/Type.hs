module Semantics.Static.Type where

import Control.Applicative
import Control.Monad.Except
import Data.List

import Semantics.Static.Common
import qualified Semantics.Core.Names as Core
import qualified Semantics.Core.Type as Core
import Syntax.Kind
import Syntax.Names
import Syntax.Type



-- [ G !- K ni A ]
--
-- G !- A >> A' in K'    K = K'
-- ----------------------------
-- G !- K ni A >> A'
checkType :: Kind -> Type -> Elaborator Core.Type
checkType k a =
  do (a',k') <- synthType a
     if k == k'
       then return a'
       else throwError (ElabError "Mismatching kinds.")



-- Type A, B ::=  a              (Type Variable)
--             |  Cn             (Type Name)
--             |  A -> B         (Function Type)
--             |  CC => A        (Constrained Type)
--             |  forall TVK. B  (Forall Type)
--             |  A B            (Type Application)

-- [ G !- A >> A' in K ]
--
-- G ni a : K
-- ----------------
-- G !- a >> a in K
--
-- G ni c :: K
-- ----------------
-- G !- c >> c in K
--
-- G !- Type ni A >> A'    G !- Type ni B >> B'
-- --------------------------------------------
-- G !- A -> B >> A' -> B' in Type
--
-- G !- CC >> CC' constraint -! G'    G, G'* !- Type ni A >> A'
-- ------------------------------------------------------------
-- G !- CC => A >> CC' => A' in Type
--
-- G, a :: K !- Type ni B >> B'
-- ----------------------------------------------------
-- G !- forall (a : K). B >> forall (a : K). B' in Type
--
-- G !- A >> A' in K    K = J_0 -> J_1    G !- J_0 ni B >> B'
-- ----------------------------------------------------------
-- G !- A B >> A' B' in J_1
synthType :: Type -> Elaborator (Core.Type, Kind)
synthType (VarT x) =
  synthVar x
synthType (NameT cn) =
  synthName cn
synthType (FunT a b) =
  do a' <- checkType TypeK a
     b' <- checkType TypeK b
     return (Core.FunT a' b', TypeK)
synthType (ConstrainedT cc a) =
  do cc' <- checkConstraint cc
     a' <- checkType TypeK a
     return (Core.ConstrainedT cc' a', TypeK)
synthType (ForallT (TyVarKinding a k) b) =
  do b' <- extendTypeVarContext a k (checkType TypeK b)
     return (Core.ForallT (Core.TyVarKinding a k) b', TypeK)
synthType (AppT a b) =
  do (a', j) <- synthType a
     case j of
       FunK j0 j1 -> do
         b' <- checkType j0 b
         return (Core.AppT a' b', j1)
       _ -> throwError (ElabError "Non-function kind.")



synthVar :: TypeVar -> Elaborator (Core.Type, Kind)
synthVar x =
  do g <- getContext
     imps <- getImports
     defs <- getDefinitions
     mdl <- getCurrentModule
     case lookupPlainVar (typeVarContext g)
      <|> lookupLocalDeclaredVar mdl (typeVarDefinitions defs)
      <|> lookupImportedVar (typeVarImports imps) (typeVarDefinitions defs)
      of
       Just ak -> return ak
       Nothing -> throwError (ElabError "Unbound type variable.")

  where

    lookupPlainVar :: TypeVarContext -> Maybe (Core.Type, Kind)
    lookupPlainVar tvg =
      do k <- lookup x tvg
         return (Core.VarT x, k)

    lookupLocalDeclaredVar :: ModulePath -> TypeVarDefinitions -> Maybe (Core.Type, Kind)
    lookupLocalDeclaredVar modpath tvdefs =
      do (d,(_,k)) <- find (currentModuleTypeVar modpath) tvdefs
         return (Core.DecVarT d, k)

    currentModuleTypeVar :: ModulePath -> (Core.DeclaredTypeVar, (Core.Type, Kind)) -> Bool
    currentModuleTypeVar modpath (d,_) =
      Core.DeclaredTypeVar modpath x == d

    lookupImportedVar :: TypeVarImports -> TypeVarDefinitions -> Maybe (Core.Type, Kind)
    lookupImportedVar tvimps tvdefs =
      do d <- lookup x tvimps
         (_,k) <- lookup d tvdefs
         return (Core.DecVarT d, k)



synthName :: TypeName -> Elaborator (Core.Type, Kind)
synthName cn =
  do imps <- getImports
     defs <- getDefinitions
     mdl <- getCurrentModule
     case lookupLocalDeclaredName mdl (typeNameDefinitions defs)
      <|> lookupImportedName (typeNameImports imps) (typeNameDefinitions defs)
      of
        Just ak -> return ak
        Nothing -> throwError (ElabError "Unknown type name.")

  where

    lookupLocalDeclaredName :: ModulePath -> TypeNameDefinitions -> Maybe (Core.Type, Kind)
    lookupLocalDeclaredName modpath tndefs =
      do (d,s) <- find (currentModuleTypeName modpath) tndefs
         return (typeFromDeclaredName d s, kindFromSignature s)

    currentModuleTypeName :: ModulePath -> (Core.DeclaredTypeName, TypeNameSignature) -> Bool
    currentModuleTypeName modpath (d,_) =
      Core.DeclaredTypeName modpath cn == d

    lookupImportedName :: TypeNameImports -> TypeNameDefinitions -> Maybe (Core.Type, Kind)
    lookupImportedName tnimps tndefs =
      do d <- lookup cn tnimps
         s <- lookup d tndefs
         return (typeFromDeclaredName d s, kindFromSignature s)

    typeFromDeclaredName :: Core.DeclaredTypeName -> TypeNameSignature -> Core.Type
    typeFromDeclaredName d (TypeNameSignature ks) =
      foldr Core.LamT (Core.ConT d (map Core.VarT xs)) (zipWith Core.TyVarKinding xs ks)
      where
        xs = [ TypeVar (VarName ("a" ++ show i)) | i <- [0..length ks - 1] ]


    kindFromSignature :: TypeNameSignature -> Kind
    kindFromSignature (TypeNameSignature ks) =
      foldr FunK TypeK ks



checkConstraint :: ClassConstraint -> Elaborator Core.ClassConstraint
checkConstraint (ClassConstraint cn xs) =
  do defs <- getDefinitions
     imps <- getImports
     mdl <- getCurrentModule
     case lookupLocalClass mdl (classDefinitions defs)
      <|> lookupImportedClass (classImports imps) (classDefinitions defs)
      of
       Nothing -> throwError (ElabError "Unknown class name.")
       Just (dcn, ClassSignature ks) ->
         if length xs /= length ks
           then throwError (ElabError "Incorrect number of class arguments.")
           else
             do zipWithM_ checkClassConstraintArg ks xs
                return $ Core.ClassConstraint dcn xs

  where

    lookupLocalClass :: ModulePath -> ClassDefinitions -> Maybe (Core.DeclaredClassName, ClassSignature)
    lookupLocalClass modpath cdefs =
      find (currentModuleClassName modpath) cdefs

    currentModuleClassName :: ModulePath -> (Core.DeclaredClassName, ClassSignature) -> Bool
    currentModuleClassName modpath (d,_) =
      Core.DeclaredClassName modpath cn == d

    lookupImportedClass :: ClassImports -> ClassDefinitions -> Maybe (Core.DeclaredClassName, ClassSignature)
    lookupImportedClass cimps cdefs =
      do dcn <- lookup cn cimps
         sig <- lookup dcn cdefs
         return (dcn, sig)


checkClassConstraintArg :: Kind -> TypeVar -> Elaborator ()
checkClassConstraintArg k x =
  do ctx <- getContext
     case lookup x (typeVarContext ctx) of
       Nothing -> throwError (ElabError "Unbound type variable in class constraint.")
       Just k' | k == k' -> return ()
               | otherwise -> throwError (ElabError "Mismatching kinds in class constraint.")
