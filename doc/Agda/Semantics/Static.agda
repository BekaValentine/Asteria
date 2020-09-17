module Semantics.Static where

open import Data.Bool
open import Data.List renaming (_∷_ to _::_)
open import Data.Maybe hiding (All ; map)

open import Syntax.AST



data All {A : Set} (P : A -> Set) : List  A -> Set where
  [] : All P []
  _::_ : forall {x xs} -> P x -> All P xs -> All P (x :: xs)

data AllZip {A B : Set} (P : A -> B -> Set) : List A -> List B -> Set where
  [] : AllZip P [] []
  _::_ : forall {x y xs ys} -> P x y -> AllZip P xs ys -> AllZip P (x :: xs) (y :: ys)



data TypeContextJudgment : Set where
  _::Ty_ : VarName -> Kind -> TypeContextJudgment
  _class_ : IdentName -> List TyVarKinding -> TypeContextJudgment
  _tycon_ : IdentName -> List TyVarKinding -> TypeContextJudgment
  _tydef_:=_ : IdentName -> List TyVarKinding -> Type -> TypeContextJudgment



tyVarKindingToTypeContextJudgment : TyVarKinding -> TypeContextJudgment
tyVarKindingToTypeContextJudgment (a ::Ty K) = a ::Ty K



data TypeContext : Set where
  <> : TypeContext
  _,_ : TypeContext -> TypeContextJudgment -> TypeContext



_++Ty_ : TypeContext -> List TypeContextJudgment -> TypeContext
G ++Ty [] = G
G ++Ty (J :: G') = (G , J) ++Ty G'



tyVarKindingsToKind : List TyVarKinding -> Kind
tyVarKindingsToKind [] = TypeK
tyVarKindingsToKind ((_ ::Ty K) :: aKs) = K ->K tyVarKindingsToKind aKs



mutual

  data NormalType : Set where
    _->NT_ : NormalType -> NormalType -> NormalType
    _=>NT_ : NormalClassConstraint -> NormalType -> NormalType
    ForallNT : TyVarKinding -> Type -> NormalType
    ConNT[_] : IdentName -> List NormalType -> NormalType
    LamNT : VarName -> NormalType -> NormalType
    Stuck : StuckType -> NormalType



  data StuckType : Set where
    VarST : VarName -> StuckType
    AppST : StuckType -> NormalType -> StuckType



  record NormalClassConstraint : Set where
    inductive
    constructor MkNormalClassConstraint
    field
      class : IdentName
      arguments : List NormalType



data TypeEnv : Set where
  [] : TypeEnv
  _,_ident=_ : TypeEnv -> IdentName -> NormalType -> TypeEnv
  _,_var=_ : TypeEnv -> VarName -> NormalType -> TypeEnv



_*=>NT_ : List NormalClassConstraint -> NormalType -> NormalType
[] *=>NT A = A
(C :: Cs) *=>NT A = C =>NT (Cs *=>NT A)



lookupTypeVar : TypeEnv -> VarName -> Maybe NormalType
lookupTypeVar [] x = nothing
lookupTypeVar (env , c ident= NA) x = lookupTypeVar env x
lookupTypeVar (env , y var= NA) x with y ==VarName x
lookupTypeVar (env , y var= NA) x | false = lookupTypeVar env x
lookupTypeVar (env , y var= NA) x | true = just NA



lookupTypeName : TypeEnv -> IdentName -> Maybe NormalType
lookupTypeName [] c = nothing
lookupTypeName (env , d ident= NA) c with d ==IdentName c
lookupTypeName (env , d ident= NA) c | false = lookupTypeName env c
lookupTypeName (env , d ident= NA) c | true = just NA
lookupTypeName (env , y var= NA) c = nothing



mutual

  -- {-# TERMINATING #-}
  normalType : TypeEnv -> Type -> Maybe Type
  normalType env (VarT x) = lookupTypeVar env x
  normalType env (A ->T B) with normalType env A | normalType env B
  ... | just NA | just NB = just (NA ->NT NB)
  ... | _ | _ = nothing
  normalType env (Cs =>T A) with normalConstraints env Cs | normalType env A
  ... | just NCs | just NA = just (NCs *=>NT NA)
  ... | _ | _ = nothing
  normalType env (ForallT aa B) = unrollForall env aa B
    where
      unrollForall : TypeEnv -> List TyVarKinding -> Type -> Maybe NormalType
      unrollForall env [] B = normalType env B
      unrollForall env ((a ::Ty K) :: aa) B = unrollForall (env , a var= Stuck (VarST a)) aa B
  normalType env (NameT n) = lookupTypeName env n
  normalType env (AppT A B) with normalType env A | normalType env B
  normalType env (AppT A B) | just (LamNT x C) | just NB = {! normalType ? NA  !}
  normalType env (AppT A B) | just NA           | just NB = {!   !}
  ... | _ | _ = nothing

  normalConstraints : TypeEnv -> List ClassConstraint -> Maybe (List NormalClassConstraint)
  normalConstraints env [] = just []
  normalConstraints env (C :: Cs) with normalConstraint env C | normalConstraints env Cs
  ... | just NC | just NCs = just (NC :: NCs)
  ... | _ | _ = nothing

  normalConstraint : TypeEnv -> ClassConstraint -> Maybe NormalClassConstraint
  normalConstraint env (MkClassConstraint cl args) = {!   !}



data HasKindIn : TypeContext -> VarName -> Kind -> Set where

  -- -------------------
  -- G, a :: K ni a :: K
  here : forall {G a K} -> HasKindIn (G , (a ::Ty K)) a K

  -- G ni a :: K
  -- -------------------
  -- G, b :: J ni a :: K
  there : forall {G a b K J} -> HasKindIn G a K -> HasKindIn (G , (b ::Ty J)) a K



data HasTypeConstructor : TypeContext -> IdentName -> List TyVarKinding -> Set where
  here : forall {G c aKs} -> HasTypeConstructor (G , (c tycon aKs)) c aKs
  there : forall {G c aKs J} -> HasTypeConstructor G c aKs -> HasTypeConstructor (G , J) c aKs



data HasTypeDef : TypeContext -> IdentName -> List TyVarKinding -> Type -> Set where
  here : forall {G n aKs A} -> HasTypeDef (G , (n tydef aKs := A)) n aKs A
  there : forall {G n aKs A J} -> HasTypeDef G n aKs A -> HasTypeDef (G , J) n aKs A



data HasTypeName (G : TypeContext) (n : IdentName) (aKs : List TyVarKinding) : Set where
  hasTypeConstructor : HasTypeConstructor G n aKs -> HasTypeName G n aKs
  hasTypeDef : forall {A} -> HasTypeDef G n aKs A -> HasTypeName G n aKs



data HasClass : TypeContext -> IdentName -> List TyVarKinding -> Set where
  here : forall {G c aKs} -> HasClass (G , (c class aKs)) c aKs
  there : forall {G c aKs J} -> HasClass G c aKs -> HasClass (G , J) c aKs



mutual

  data HasKind (G : TypeContext) : Type -> Kind -> Set where

    -- G ni a :: K
    -- -----------
    -- G !- a :: K
    TyVar : forall {a K} -> HasKindIn G a K -> HasKind G (VarT a) K

    -- G !- A :: Type   G !- B :: Type
    -- -------------------------------
    --       G !- A -> B :: Type
    TyFun : forall {A B} -> HasKind G A TypeK -> HasKind G B TypeK -> HasKind G (A ->T B) TypeK

    -- G !- C_i constraint   G !- A :: Type
    -- ------------------------------------
    --       G !- (C*) => A :: Type
    TyConstraint : forall {Cs A} -> All (IsConstraint G) Cs -> HasKind G A TypeK -> HasKind G (Cs =>T A) TypeK

    -- G, (a :: K)* !- B :: Type
    -- -------------------------------
    -- G !- forall (a : K)*. B :: Type
    TyForall : forall {aKs B} -> HasKind (G ++Ty map tyVarKindingToTypeContextJudgment aKs) B TypeK -> HasKind G (ForallT aKs B) TypeK

    -- G ni c names (a :: K)*
    -- ----------------------------
    -- G !- c :: K_0 -> ... -> Type
    TyName : forall {c aKs} -> HasTypeName G c aKs -> HasKind G (NameT c) (tyVarKindingsToKind aKs)

    -- G !- A :: J -> K   G !- B :: J
    -- ------------------------------
    --         G !- A B :: K
    TyApp : forall {A B J K} -> HasKind G A (J ->K K) -> HasKind G B J -> HasKind G (AppT A B) K



  data IsConstraint (G : TypeContext) (C : ClassConstraint) : Set where
    isConstraint : forall {csig}
                -> HasClass G (ClassConstraint.class C) csig
                -> AllZip (\ { (a ::Ty K) A -> HasKind G A K }) csig (ClassConstraint.arguments C)
                -> IsConstraint G C
