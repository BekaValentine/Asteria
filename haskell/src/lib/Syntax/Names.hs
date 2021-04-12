module Syntax.Names where

-- [a-z][a-zA-Z0-9_]*
newtype VarName = VarName String
  deriving (Show,Eq)

-- [A-Z][a-zA-Z0-9_]*
newtype IdentName = IdentName String
  deriving (Show,Eq)

newtype ModuleName = ModuleName IdentName
  deriving (Show,Eq)

newtype ModulePath = ModulePath [ModuleName]
  deriving (Show,Eq)

newtype TypeVar  = TypeVar VarName
  deriving (Show,Eq)

newtype TypeName = TypeName IdentName
  deriving (Show,Eq)

newtype ClassName = ClassName IdentName
  deriving (Show,Eq)

newtype TermVar = TermVar VarName
  deriving (Show,Eq)

newtype TermName = TermName IdentName
  deriving (Show,Eq)
