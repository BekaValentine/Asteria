module Syntax.AST where

open import Data.Bool
open import Data.List



postulate IdentName : Set
postulate _==IdentName_ : IdentName -> IdentName -> Bool
postulate VarName : Set
postulate _==VarName_ : VarName -> VarName -> Bool






data Kind : Set where

  -- Type
  TypeK : Kind

  -- K -> J
  _->K_ : Kind -> Kind -> Kind



mutual

  data Type : Set where

    -- a
    VarT : VarName -> Type

    -- A -> B
    _->T_ : Type -> Type -> Type

    -- (Foo a b) => c
    _=>T_ : List ClassConstraint -> Type -> Type

    -- forall (a : A) (b : B). C
    ForallT : List TyVarKinding -> Type -> Type

    -- Foo
    NameT : IdentName -> Type

    -- A B
    AppT : Type -> Type -> Type



  -- (a : Type)
  record TyVarKinding : Set where
    constructor _::Ty_
    field
      name : VarName
      kind : Kind



  -- (Foo a b c)
  record ClassConstraint : Set where
    inductive
    constructor MkClassConstraint
    field
      class : IdentName
      arguments : List Type



-- (x : A)
record ConstructorParameter : Set where
  constructor MkConstructorParameter
  field
    name : VarName
    type : Type



-- Foo (x : A) (y : B) : C
record ConstructorDecl : Set where
  constructor MkConstructorDecl
  field
    name : IdentName
    parameters : List ConstructorParameter
    constructedType : Type



-- foo : A -> B -> C
record MethodDeclaration : Set where
  constructor MkMethodDeclaration
  field
    name : VarName
    type : Type



data Pattern : Set where

  -- x
  VarPat : VarName -> Pattern

  -- Foo x y z
  ConPat : IdentName -> List Pattern -> Pattern



mutual

  data Term : Set where

    -- M : A
    AnnTm : Term -> Type -> Term

    -- x
    VarTm : VarName -> Term

    -- \x y z -> M
    LamTm : List VarName -> Term -> Term

    -- M N
    AppTm : Term -> Term -> Term

    -- case M where
    --   x | test1 | test2 -> M
    CaseTm : Term -> List CaseClause -> Term

    -- let ...
    -- in N
    LetTm : List Declaration -> Term -> Term



  -- x | test1 | test2 -> M
  record CaseClause : Set where
    inductive
    constructor MkCaseClause
    field
      matchPattern : Pattern
      guards : List Term
      body : Term



  data Declaration : Set where

    -- data List (a : Type) where
    --   Nil : List a
    --   Cons (x : a) (xs : List a) : List a
    DataDecl : IdentName -> List TyVarKinding -> List ConstructorDecl -> Declaration

    -- type Foo = List Int
    TypeSynonymDecl : IdentName -> List VarName -> Type -> Declaration

    -- class Foo (a : Type) <= (Bar a, Baz a) where
    --   m0 : a
    --   m1 : a -> a
    ClassDecl : IdentName -> List TyVarKinding -> List ClassConstraint -> List MethodDeclaration -> Declaration

    -- foo : A -> B -> C
    TermTypeSig : VarName -> Type -> Declaration

    -- foo (Pat x) (Pat y) | test1 | test2 = z
    TermEquation : VarName -> List Pattern -> List Term -> Term -> Declaration



data DeclaredName : Set where
  identName : IdentName -> DeclaredName
  varName : VarName -> DeclaredName



data UsingHiding : Set where

  -- neither using nor hiding
  usingAll : UsingHiding

  -- using (foo, bar, baz)
  usingOnly : List DeclaredName -> UsingHiding

  -- hiding (foo, bar, baz)
  hidingOnly : List DeclaredName -> UsingHiding


data Renaming : Set where

  -- Foo to Bar
  renameIdent : IdentName -> IdentName -> Renaming

  -- foo to bar
  renameVar : VarName -> VarName -> Renaming

record Import : Set where
  constructor MkImport
  field
    importedName : IdentName
    isQualified : Bool
    usingHiding : UsingHiding
    renamings : List Renaming




-- module (t : Type)
--   import Foo.Bar
--   import qualified Baz using (x, y, z)
-- where
--     ...
record Module : Set where
  constructor MkModule
  field
    parameters : List TyVarKinding
    imports : List Import
    declarations : List Declaration



-- Foo.Bar.Baz = module ...
record NamedModule : Set where
  constructor MkNamedModule
  field
    name : List IdentName
    rawModule : Module



-- Foo.Bar.Baz = module ...
-- Foo.Bar.Quux = module ...
-- ...
Program : Set
Program = List NamedModule
