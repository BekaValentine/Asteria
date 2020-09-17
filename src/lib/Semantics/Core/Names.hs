module Semantics.Core.Names where

import Syntax.Names




newtype MetaVar = MetaVar Int
  deriving (Show,Eq)


data DeclaredTermName = DeclaredTermName ModulePath TermName
  deriving (Show,Eq)

data DeclaredTypeName = DeclaredTypeName ModulePath TypeName
  deriving (Show,Eq)

data DeclaredClassName = DeclaredClassName ModulePath ClassName
  deriving (Show,Eq)



data DeclaredTermVar = DeclaredTermVar ModulePath TermVar [TermVar]
  deriving (Show,Eq)

data DeclaredTypeVar = DeclaredTypeVar ModulePath TypeVar
  deriving (Show,Eq)
