module Syntax.Names where

newtype VarName = VarName String
newtype IdentName = IdentName String

newtype ModuleName = ModuleName IdentName
newtype TypeVar  = TypeVar VarName
newtype TypeName = TypeName IdentName
newtype ClassName = ClassName IdentName
newtype TermVar = TermVar VarName
newtype ConstructorName = ConstructorName IdentName
