module Syntax.Pattern where

import Syntax.Names



data Pattern = VarPat TermVar
             | ConPat ConstructorName [Pattern]
