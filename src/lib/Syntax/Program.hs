module Syntax.Program where

import Syntax.Module
import Syntax.Names



newtype Program = Program [NamedModule]
  deriving (Show,Eq)

data NamedModule = NamedModule ModulePath RawModule
  deriving (Show,Eq)
