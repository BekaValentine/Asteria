{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Semantics.Static.Common where

import Control.Monad.Reader
import Control.Monad.State

import Semantics.Core.Names
import qualified Semantics.Core.Term as Core
import qualified Semantics.Core.Type as Core
import Syntax.Kind
import Syntax.Names




type TermVarContext = [(TermVar, Core.Type)]
type TypeVarContext = [(TypeVar, Kind)]

data Context =
  Context
    { termVarContext :: TermVarContext
    , typeVarContext :: TypeVarContext
    }
  deriving (Show)



type TermVarDefinitions = [(DeclaredTermVar, (Core.Term, Core.Type))]
type TermNameDefinitions = [(DeclaredTermName, TermNameSignature)]
type TypeVarDefinitions = [(DeclaredTypeVar, (Core.Type, Kind))]
type TypeNameDefinitions = [(DeclaredTypeName, TypeNameSignature)]
type ClassDefinitions = [(DeclaredClassName, ClassSignature)]

data Definitions =
  Definitions
    { termVarDefinitions :: TermVarDefinitions
    , termNameDefinitions :: TermNameDefinitions
    , typeVarDefinitions :: TypeVarDefinitions
    , typeNameDefinitions :: TypeNameDefinitions
    , classDefinitions :: ClassDefinitions
    }
  deriving (Show)

data TermNameSignature = TermNameSignature [TypeVar] [Core.Type] Core.Type
  deriving (Show)

data TypeNameSignature = TypeNameSignature [Kind]
  deriving (Show)

data ClassSignature =
  ClassSignature
    { parameters :: [Kind]
    }
  deriving (Show,Eq)



type TermVarImports = [(TermVar, DeclaredTermVar)]
type TermNameImports = [(TermName, DeclaredTermName)]
type TypeVarImports = [(TypeVar, DeclaredTypeVar)]
type TypeNameImports = [(TypeName, DeclaredTypeName)]
type ClassImports = [(ClassName, DeclaredClassName)]

data Imports =
  Imports
    { termVarImports :: TermVarImports
    , termNameImports :: TermNameImports
    , typeVarImports :: TypeVarImports
    , typeNameImports :: TypeNameImports
    , classImports :: ClassImports
    }
  deriving (Show)



type Elaborator a = ReaderT ElabContext (StateT ElabState (Either ElabError)) a



data ElabContext =
  ElabContext
    { currentModule :: ModulePath
    , imports :: Imports
    , context :: Context
    }
  deriving (Show)



type Unifications = [(MetaVar,Core.Type)]



data ElabState =
  ElabState
    { definitions :: Definitions
    }
  deriving (Show)



data ElabError = ElabError String
  deriving (Show)





getCurrentModule :: Elaborator ModulePath
getCurrentModule = currentModule <$> ask

getImports :: Elaborator Imports
getImports = imports <$> ask

getContext :: Elaborator Context
getContext = context <$> ask

getDefinitions :: Elaborator Definitions
getDefinitions = definitions <$> get

extendTypeVarContext :: TypeVar -> Kind -> Elaborator a -> Elaborator a
extendTypeVarContext a k = local (\ec -> ec { context = (context ec) { typeVarContext = typeVarContext (context ec) } })
