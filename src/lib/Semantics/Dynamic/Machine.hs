{-# OPTIONS -Wall #-}

module Semantics.Dynamic.Machine where

import Syntax.Module
import Syntax.Names
import Syntax.Pattern
import Syntax.Program
import Syntax.Term



programToDeclarations :: Program -> Declarations
programToDeclarations (Program mods) =
  do NamedModule mp rm <- mods
     rawModuleToDeclarations mp rm



rawModuleToDeclarations :: ModulePath RawModule -> Declarations
rawModuleToDeclarations mp (RawModule _ _ _) = _


-- Machine Mm ::=  D;K;E > M  (Entering Machine)
--              |  D;K;E < V  (Exiting Machine)
data Machine = Entering Declarations Stack Environment Term
             | Exiting Declarations Stack Environment Value
             | Done Value
             | Error String
  deriving (Show,Eq)



-- Stack K ::=  F*  (Stack)
type Stack = [Frame]



-- Frame F ::=  _ N                                   (In Application Left)
--           |  V _                                   (In Application Right)
--           |  case V*, _, M* of Cls*                (In Case)
--           |  let { x* = V*; x = _; x* = M* } in N  (In Let)
data Frame = InAppLeft Term
           | InAppRight Value
           | InCase [Value] [Term] [CaseClause]
           | InLetDecl Environment TermVar [(TermVar,Term)] Term
           | Bind Int
  deriving (Show,Eq)



-- Declarations D ::=  (x = V)*  (Declarations)
type Declarations = [(String, Term)]



-- Environment E ::=  (x|Cn = V)*  (Environment)
type Environment = [(String, Value)]



-- Value U, V ::=  ConV V*      (Constructor Value)
--              |  clo[E] x. M  (Closure)
data Value = ConV TermName [Value]
           | CloV Environment TermVar Term
  deriving (Show,Eq)



-- D;K;E > M : A                -->   D;K;E > M
-- D;K;E > x                    -->   D;K;E < E[e]
-- D;K;E > Cn                   -->   D;K;E < E[Cn]
-- D;K;E > \x+ -> M             -->   D;K;E < clo[E] x. \x* -> M
-- D;K;E > M N                  -->   D;K, _ N; E > M
-- D;K;E > M @ A                -->   D;K;E > M
-- D;K;E > case M, M* of Cls*   -->   D;K, case _, M* of Cls*; E > M
-- D;K;E > let { x* = V*        -->   D;K, let { x* = V  > M
--             ; x = M                         ; x = _
--             ; x* = M*                       ; x* = M*
--             } in N                          } in N
-- D;K;E > define { x* = M*     -->   D, x* = M; K; E > N
--                } in N
step :: Machine -> Machine

step (Entering d k e (Ann m _)) =
  Entering d k e m
step (Entering d k e (Var x)) =
  case lookupVariable e x of
    Nothing -> Error "Whoops! I somehow borked up and gave you a free variable."
    Just v -> Exiting d k e v
step (Entering d k e (Name cn)) =
  case lookupName e cn of
    Nothing -> Error "Whoops! I somehow borked up and gave you a free name."
    Just v -> Exiting d k e v
step (Entering d k e (Lam xs m)) =
  case xs of
    [] -> Error "Whoops! I somehow borked up and gave you a lambda with no parameters."
    [x] -> Exiting d k e (CloV e x m)
    x:xs' -> Exiting d k e (CloV e x (Lam xs' m))
step (Entering d k e (App m n)) = Entering d (InAppLeft n : k) e m
step (Entering d k e (Inst m _)) = Entering d k e m
step (Entering d k e (Case ms cls)) =
  case ms of
    [] -> Error "Whoops! I somehow borked up and gave you a case expression with no scrutinees."
    m:ms' -> Entering d (InCase [] ms' cls:k) e m
step (Entering d k e (Let decls n)) =
  case gatherEquations decls of
    Just ((x,m):eqs) -> Entering d (InLetDecl [] x eqs n:k) e m
    _ -> Error "Whoops! I somehow borked up and gave you a let expression with no equations."
step (Entering d k e (Def decls n)) =
  case gatherEquations decls of
    Nothing -> Error "Whoops! I somehow borked up and gave you a define expression with no equations."
    Just defs -> Entering ([ (x,m) | (TermVar (VarName x), m) <- defs] ++ d) k e n

{-
-- Frame F ::=  _ N                                   (In Application Left)
--           |  V _                                   (In Application Right)
--           |  case V*, _, M* of Cls*                (In Case)
--           |  let { x* = V*; x = _; x* = M* } in N  (In Let)
data Frame = InAppLeft Term
           | InAppRight Value
           | InCase [Value] [Term] [CaseClause]
           | InLetDecl Environment TermVar [LetDecl] Term
  deriving (Show,Eq)
-}
step (Exiting _ [] _ v) = Done v
step (Exiting d (InAppLeft n:k) e v) = Entering d (InAppRight v:k) e n
step (Exiting d (InAppRight u:k) e v) =
  case u of
    CloV e' (TermVar (VarName x)) m -> Entering d (Bind (1 + length e'):k) ((x,v) : e' ++ e) m
    _ -> Error "Whoops! I somehow borked up and gave you a non-closure."
step (Exiting d (InCase vs ms cls:k) e v) =
  case ms of
    [] -> case caseMatch (reverse (v:vs)) cls of
      Nothing -> Error "Whoops! I somehow borked up and gave you a non-total case expression."
      Just (e', n) -> Entering d (Bind (length e'):k) (e' ++ e) n
    m:ms' -> Entering d (InCase (v:vs) ms' cls:k) e m
step (Exiting d (InLetDecl e' (TermVar (VarName x)) ms n:k) e v) =
  case ms of
    [] -> Entering d (Bind (1 + length e'):k) ((x,v):e' ++ e) n
    (TermVar (VarName y),m):ms' -> Entering d (InLetDecl ((x,v):e') (TermVar (VarName y)) ms' n:k) ((x,v):e' ++ e) m
step (Exiting d (Bind n:k) e v) =
  Exiting d k (drop n e) v

step (Done v) = Done v
step (Error e) = Error e



lookupVariable :: Environment -> TermVar -> Maybe Value
lookupVariable e (TermVar (VarName vn)) = lookup vn e

lookupName :: Environment -> TermName -> Maybe Value
lookupName e (TermName (IdentName cn)) = lookup cn e

caseMatch :: [Value] -> [CaseClause] -> Maybe (Environment, Term)
caseMatch _ [] = Nothing
caseMatch vs (cl:cls) =
  case clauseMatch vs cl of
    Nothing -> caseMatch vs cls
    r -> r

clauseMatch :: [Value] -> CaseClause -> Maybe (Environment, Term)
clauseMatch vs (CaseClause pats n) =
  do e' <- patternSequenceMatch vs pats
     return (e', n)

patternSequenceMatch :: [Value] -> [Pattern] -> Maybe Environment
patternSequenceMatch [] [] = return []
patternSequenceMatch (v:vs) (p:ps) =
  do e' <- patternMatch v p
     e'' <- patternSequenceMatch vs ps
     return $ e' ++ e''
patternSequenceMatch _ _ = Nothing

patternMatch :: Value -> Pattern -> Maybe Environment
patternMatch v (VarPat (TermVar (VarName x))) = return [(x,v)]
patternMatch (ConV cn vs) (ConPat cn' ps) =
  if cn == cn'
  then
    patternSequenceMatch vs ps
  else
    Nothing
patternMatch _ _ = Nothing

gatherEquations :: [LocalDecl] -> Maybe [(TermVar,Term)]
gatherEquations decls =
  sequence definitions
  where
    declaredNames :: [TermVar]
    declaredNames = [ x | LocalTermTypeSig x _ <- decls ]

    equations :: TermVar -> Maybe [([Pattern],Term)]
    equations x =
      case [ (pats,m) | LocalTermEquation x' pats m <- decls, x == x' ] of
        [] -> Nothing
        eqs -> Just eqs

    definitions :: [Maybe (TermVar,Term)]
    definitions =
      do x <- declaredNames
         return $ do
           eqs <- equations x
           m <- buildTerm eqs
           return (x, m)

    buildTerm :: [([Pattern], Term)] -> Maybe Term
    buildTerm [] = Nothing
    buildTerm pats@((pat0,_):_) =
      let xs = [ TermVar (VarName ("_generated_" ++ show i)) | i <- [0..length pat0 - 1] ]
      in Just (Lam xs (Case (map Var xs) [ CaseClause pat m | (pat,m) <- pats ]))
