{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Clash.Core.Evaluator.Models where

import Prelude hiding (lookup, pi)

import Control.Concurrent.Supply (Supply)
import Control.DeepSeq (NFData(..))
import Data.Bifunctor (first, second)
import Data.Foldable (foldl')
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import GHC.Generics (Generic)

import BasicTypes (InlineSpec(..))
import SrcLoc (noSrcSpan)

import Clash.Core.DataCon
import Clash.Core.Evaluator.Delay (Delay)
import Clash.Core.Literal
import Clash.Core.Subst
import Clash.Core.Term
import Clash.Core.TyCon
import Clash.Core.Type
import Clash.Core.Util
import Clash.Core.Var
import Clash.Core.VarEnv
import Clash.Driver.Types

type EvalPrim = Env -> PrimInfo -> [Either Value Type] -> Delay Value

type EnvGlobals = VarEnv (Binding (Either Term Value))
type EnvPrims   = (IntMap Value, Int)

data Env = Env
  { envPrimEval :: EvalPrim
  , envTypes    :: Map TyVar Type
  , envLocals   :: Map Id (Either Term Value)
  , envGlobals  :: EnvGlobals
  , envPrims    :: EnvPrims
  , envTcMap    :: TyConMap
  , envInScope  :: InScopeSet
  , envSupply   :: Supply
  }

instance Show Env where
  show e = show (envLocals e, envTypes e)

instance NFData Env where
  rnf (Env _ ls ts gs ps tcm _ _) =
    rnf ls `seq` rnf ts `seq` rnf gs `seq` rnf ps `seq` rnf tcm

mkEnv
  :: EvalPrim
  -> EnvPrims
  -> BindingMap
  -> TyConMap
  -> InScopeSet
  -> Supply
  -> Env
mkEnv eval ps bm =
  Env eval mempty mempty gs ps
 where
  gs = fmap (fmap Left) bm

-- TODO This is likely not the correct substitution. We should make sure that
-- the InScopeSet and substitutions are correct.
--
asSubst :: Env -> Subst
asSubst env = subst2
 where
  subst0 = mkSubst (envInScope env)
  subst1 = foldr (\(i,ty) s -> extendTvSubst s i ty) subst0 (Map.toList $ envTypes env)
  subst2 = extendIdSubstList subst1 (Map.toList . fmap asTerm $ envLocals env)

-- | Add a new term / value to the environment. The unique for the new
-- element is guaranteed to be unique within the environment.
--
-- For new global bindings, we assume that it is possible to inline
-- them later during evaluation.
--
insertEnv :: IdScope -> Id -> Either Term Value -> Env -> Env
insertEnv scope i etv env =
  case scope of
    LocalId  -> env
      { envLocals = Map.insert i' etv (envLocals env)
      , envInScope = extendInScopeSet (envInScope env) i'
      }

    GlobalId -> 
      let b = Binding i' noSrcSpan Inline etv
       in env
      { envGlobals = extendVarEnv i' b (envGlobals env)
      , envInScope = extendInScopeSet (envInScope env) i'
      }
 where
  i' = uniqAway (envInScope env) i

insertAllEnv :: IdScope -> [(Id, Either Term Value)] -> Env -> Env
insertAllEnv s xs env = foldr (uncurry $ insertEnv s) env xs

-- | Add a new type to the environment. The unique for the new
-- element is guaranteed to be unique within the environment.
--
insertEnvTy :: TyVar -> Type -> Env -> Env
insertEnvTy i ty env =
  env
    { envTypes = Map.insert i' ty (envTypes env)
    , envInScope = extendInScopeSet (envInScope env) i'
    }
 where
  i' = uniqAway (envInScope env) i

insertAllEnvTys :: [(TyVar, Type)] -> Env -> Env
insertAllEnvTys xs env = foldr (uncurry insertEnvTy) env xs

-- | Add a new primitive to the environment. Primitives are keyed by an
-- integer ID in the evaluator. If the prim already exists in the environment,
-- you should call 'updateEnvPrim' instead.
--
insertEnvPrim :: Value -> Env -> Env
insertEnvPrim v env =
   env { envPrims = (IntMap.insert n v pm, n + 1) }
 where
  (pm, n) = envPrims env

-- | Delete the element with the speicified Id from the environment.
--
deleteEnv :: IdScope -> Id -> Env -> Env
deleteEnv scope i env =
  case scope of
    LocalId -> env { envLocals = Map.delete i (envLocals env) }
    GlobalId -> env { envGlobals = delVarEnv (envGlobals env) i }

updateEnvPrim :: Int -> Value -> Env -> Env
updateEnvPrim i v env =
  env { envPrims = (IntMap.insert i v pm, n) }
 where
  (pm, n) = envPrims env

-- | Neutral terms cannot be reduced, as they represent things like variables
-- which are unknown, partially applied functions, or case expressions where
-- the scrutinee is not yet an inspectable value. Consider:
--
-- v              Stuck if "v" is a free variable
-- x $ y          Stuck if "x" is not known to be a lambda
-- x @ A          Stuck if "x" is not known to be a type lambda
-- case x of ...  Stuck if "x" is neutral (cannot choose an alternative)
--
data Neutral a
  = NeVar   (Var Term)
  | NeApp   (Neutral a) a
  | NeTyApp (Neutral a) Type
  | NeCase  a Type [(Pat, a)]
  deriving (Show, Generic, NFData)

-- A term which has been normalised to weak head normal form (WHNF). This has
-- no redexes at the head of the term, but subterms may still contain redexes.
--
data Value
  = VNeu    (Neutral Value)
  | VData   DataCon  [Either Value Type]
  | VLit    Literal
  | VPrim   PrimInfo [Either Value Type]
  | VLam    Id Term Env
  | VTyLam  TyVar Term Env
  | VCast   Value Type Type
  | VTick   Value TickInfo
  deriving (Show, Generic, NFData)

collectValueTicks :: Value -> (Value, [TickInfo])
collectValueTicks = go []
 where
  go acc (VTick v ti) = go (ti:acc) v
  go acc v = (v, acc)

addTicks :: Value -> [TickInfo] -> Value
addTicks = foldl' VTick

-- | A term which is in head normal form (HNF). This has no redexes, and all
-- partially applied functions in subterms are eta-expanded.
--
data Nf
  = NNeu    (Neutral Nf)
  | NData   DataCon [Either Nf Type]
  | NLit    Literal
  | NPrim   PrimInfo [Either Nf Type]
  | NLam    Id Nf
  | NTyLam  TyVar Nf
  | NCast   Nf Type Type
  | NTick   Nf TickInfo
  deriving (Show, Generic, NFData)

-- Embedding WHNF and HNF values back into Term.
--
class AsTerm a where
  asTerm :: a -> Term

instance AsTerm Term where
  asTerm = id

instance (AsTerm a) => AsTerm (Neutral a) where
  asTerm (NeVar v)        = Var v
  asTerm (NeApp x y)      = App (asTerm x) (asTerm y)
  asTerm (NeTyApp x ty)   = TyApp (asTerm x) ty
  asTerm (NeCase x ty as) = Case (asTerm x) ty (second asTerm <$> as)

instance AsTerm Value where
  asTerm (VNeu n)         = asTerm n
  asTerm (VData dc args)  = mkApps (Data dc) (first asTerm <$> args)
  asTerm (VLit l)         = Literal l
  asTerm (VPrim pi args)  = mkApps (Prim pi) (first asTerm <$> args)
  asTerm (VLam x e env)   = substTm "asTerm.VLam" (asSubst env) (Lam x e)
  asTerm (VTyLam x e env) = substTm "asTerm.VTyLam" (asSubst env) (TyLam x e)
  asTerm (VCast x a b)    = Cast (asTerm x) a b
  asTerm (VTick x ti)     = Tick ti (asTerm x)

instance AsTerm Nf where
  asTerm (NNeu n)         = asTerm n
  asTerm (NData dc args)  = mkApps (Data dc) (first asTerm <$> args)
  asTerm (NLit l)         = Literal l
  asTerm (NPrim pi args)  = mkApps (Prim pi) (first asTerm <$> args)
  asTerm (NLam x e)       = Lam x (asTerm e)
  asTerm (NTyLam x e)     = TyLam x (asTerm e)
  asTerm (NCast x a b)    = Cast (asTerm x) a b
  asTerm (NTick x ti)     = Tick ti (asTerm x)

instance (AsTerm a, AsTerm b) => AsTerm (Either a b) where
  asTerm = either asTerm asTerm

