module Syntax.Program where

import Syntax.Names
import Syntax.Pattern
import Syntax.Term
import Syntax.Type



newtype Program = Program [NamedModule]

data NamedModule = NamedModule ModuleName RawModule



-- RawModule RMod ::=  module TVK* Imp* where Decl*  (Raw Module)
data RawModule = RawModule [TyVarKinding] [Import] [Declaration]



-- Import Imp ::=  import MName ImpAs? UH? Ren?
data Import = Import ModuleName ImportAs UsingHiding [Renaming]



-- ImportAs ImpAs ::=
data ImportAs = NoLocalName

                -- |  as MName  (Import As)
              | UseLocalName ModuleName



-- UsingHiding UH    ::=
data UsingHiding = UsingAll

                   -- |  using (DN*)   (Using)
                 | UsingOnly [DeclaredName]

                   -- |  hiding (DN*)  (Hiding)
                 | HidingOnly [DeclaredName]



-- Renaming Ren ::=  Ident to Ident  (Rename Identifier)
--                |  Var to Var      (Rename Variable)
data Renaming = RenameIdent IdentName IdentName
              | RenameVar VarName VarName



-- DeclaredName Dn
data DeclaredName = DeclaredIdent IdentName
                  | DeclaredVar VarName



-- Declaration Decl
--   ::=  data Cn TVK* where ConDecl*             (Data Declaration)
--     |  type f a* = A                           (Type Synonym Declaration)
--     |  class CCn TVK* <= (CC*) where MetDecl*  (Type Class Declaration)
--     |  x : A                                   (Term Type Signature)
--     |  x Pat* = M                              (Term Equation)
data Declaration
  = DataDecl TypeName [TyVarKinding] [ConstructorDecl]
  | TypeSynonymDecl TypeVar [TypeVar] Type
  | ClassDecl ClassName [TyVarKinding] [ClassConstraint] [MethodDeclaration]
  | TermTypeSig TermVar Type
  | TermEquation TermVar [Pattern] [Term] Term



-- ConstructorDecl ConDecl ::=  Cn VK : A  (Constructor Declaration)
data ConstructorDecl = ConstructorDecl ConstructorName VarTyping Type



-- VarTyping VT ::=  x : A  (Variable Typing)
data VarTyping = VarKinding TermVar Type



-- MethodDeclaration MetDecl ::=  x : A  (Method Declaration)
data MethodDeclaration = MethodDeclaration TermVar Type
