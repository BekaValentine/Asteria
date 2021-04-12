module Syntax.Module where

import Syntax.Names
import Syntax.Pattern
import Syntax.Term
import Syntax.Type



-- RawModule RMod ::=  module TVK* Imp* where Decl*  (Raw Module)
data RawModule = RawModule [Import] [Declaration]
  deriving (Show,Eq)



-- Import Imp ::=  import MName ImpAs? UH? Rens?
data Import = Import ModulePath ImportAs UsingHiding [Renaming]
  deriving (Show,Eq)



-- ImportAs ImpAs ::=
data ImportAs = NoLocalName

                -- |  as MName  (Import As)
              | UseLocalName ModuleName
  deriving (Show,Eq)



-- UsingHiding UH    ::=
data UsingHiding = UsingAll

                   -- |  using (DN*)   (Using)
                 | UsingOnly [DeclaredName]

                   -- |  hiding (DN*)  (Hiding)
                 | HidingOnly [DeclaredName]
  deriving (Show,Eq)



-- Renamings Rens ::=  renaming (Ren+)  (Renamings)
-- Renaming Ren ::=  Ident to Ident  (Rename Identifier)
--                |  Var to Var      (Rename Variable)
data Renaming = RenameIdent IdentName IdentName
              | RenameVar VarName VarName
  deriving (Show,Eq)



-- DeclaredName Dn
data DeclaredName = DeclaredIdent IdentName
                  | DeclaredVar VarName
  deriving (Show,Eq)



-- Declaration Decl
--   ::=  data Cn TVK* where ConDecl*             (Data Declaration)
--     |  type f TVK* = A                         (Type Synonym Declaration)
--     |  class CCn TVK* <= (CC*) where MetDecl*  (Type Class Declaration)
--     |  x : A                                   (Term Type Signature)
--     |  x Pat* = N                         (Term Equation)
--     |  instance CCn A+ <= (CC*) where MetEq*   (Instance Declaration)
data Declaration
  = DataDecl TypeName [TyVarKinding] [ConstructorDecl]
  | TypeSynonymDecl TypeVar [TyVarKinding] Type
  | ClassDecl ClassName [TyVarKinding] [ClassConstraint] [MethodDeclaration]
  | TermTypeSig TermVar Type
  | TermEquation TermVar [Pattern] Term
  | InstanceDecl ClassName [Type] [ClassConstraint] [MethodEquation]
  deriving (Show,Eq)



-- ConstructorDecl ConDecl ::=  Cn VK* : A  (Constructor Declaration)
data ConstructorDecl = ConstructorDecl TermName [VarTyping] Type
  deriving (Show,Eq)



-- VarTyping VT ::=  x : A  (Variable Typing)
data VarTyping = VarTyping TermVar Type
  deriving (Show,Eq)



-- MethodDeclaration MetDecl ::=  x : A  (Method Declaration)
data MethodDeclaration = MethodDeclaration TermVar Type
  deriving (Show,Eq)



-- MethodEquation MetEq ::=  x Pat* = N  (Method Equation)
data MethodEquation = MethodEquation TermVar [Pattern] Term
  deriving (Show,Eq)
