{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TupleSections #-}

module Clash.Core.Evaluator.Semantics
  ( partialEval
  , evaluate
  , quote
  ) where

import Prelude hiding (pi)

import Control.Concurrent.Supply (Supply)
import qualified Control.Monad.State.Strict as State
import Data.Bitraversable (bitraverse)
import qualified Data.Either as Either
import qualified Data.Map.Strict as Map

import BasicTypes (InlineSpec(..))

import Clash.Core.DataCon
import Clash.Core.Evaluator.Models
import Clash.Core.Term
import Clash.Core.TyCon
import Clash.Core.Type
import Clash.Core.Var
import Clash.Core.VarEnv
import Clash.Driver.Types

-- TODO: In practice this may not terminate. If this ends up being a problem
-- we should impose a limit on how many times we are acceptable with being
-- delayed - returning the original subterm if we exceed this.
--
partialEval
  :: EvalPrim
  -> EnvPrims
  -> BindingMap
  -> TyConMap
  -> InScopeSet
  -> Supply
  -> Term
  -> (Term, BindingMap, EnvPrims)
partialEval eval ps bm tcm is ids x =
  let (nf, env') = State.runState (evaluate x >>= quote) env
   in (asTerm nf, fmap asTerm <$> envGlobals env', envPrims env')
 where
  env = mkEnv eval ps bm tcm is ids
{-# SCC partialEval #-}

-- TODO Currently, a globally bound term which is not WHNF is re-evaluated
-- every time it is looked up in the environment. We should keep this result
-- so we only evaluate each global once.
--
-- Does this mean changing Eval to StateT Env Eval ?
--
evaluate :: Term -> Eval Value
evaluate = \case
  Var v -> evaluateVar v
  Data dc -> return (VData dc [])
  Literal l -> return (VLit l)
  Prim pi -> return (VPrim pi [])
  Lam x e -> State.gets (VLam x e)
  TyLam x e -> State.gets (VTyLam x e)
  App x y -> evaluateApp x y
  TyApp x ty -> evaluateTyApp x ty
  Letrec bs e -> evaluateLetrec bs e
  Case e ty xs -> evaluateCase e ty xs
  Cast x a b -> evaluateCast x a b
  Tick ti x -> evaluateTick x ti
{-# SCC evaluate #-}

evaluateVar :: Id -> Eval Value
evaluateVar i = do
  localEnv  <- State.gets envLocals
  globalEnv <- State.gets envGlobals

  if | Just etv <- Map.lookup i localEnv
     -> go LocalId etv

     | Just b <- lookupVarEnv i globalEnv
     , bindingSpec b == Inline || bindingSpec b == Inlinable
     -> go GlobalId (bindingTerm b)

     | otherwise
     -> return $ VNeu (NeVar i)
 where
  -- A variable that refers to a previously unevaluated term is evaluated
  -- on lookup in an environment that doesn't contain that variable. This
  -- is to stop the evaluator looping when looking up a self-recursive
  -- definition (i.e. recursive let bindings).
  --
  go s = either (State.withState (deleteEnv s i) . evaluate) return
{-# SCC evaluateVar #-}

evaluateApp :: Term -> Term -> Eval Value
evaluateApp x y = do
  evalX <- evaluate x
  evalY <- evaluate y
  evalApplication Left apply evalX evalY
{-# SCC evaluateApp #-}

evaluateTyApp :: Term -> Type -> Eval Value
evaluateTyApp x ty = do
  evalX <- evaluate x
  evalApplication Right applyTy evalX ty
{-# SCC evaluateTyApp #-}

evalApplication
  :: (r -> Either Value Type)
  -> (Value -> r -> Eval Value)
  -> Value
  -> r
  -> Eval Value
evalApplication toArg f x y = do
  case x of
    VData dc args -> applyToData dc (args <> [toArg y])
    VPrim pi args -> applyToPrim pi (args <> [toArg y])
    _ -> f x y
 where
  applyToData dc args =
    case compare (length args) (length tys) of
      LT -> return (VData dc args)
      EQ -> return (VData dc args)
      GT -> error "applyToData: Overapplied DC"
   where
    tys = fst $ splitFunForallTy (dcType dc)

  applyToPrim pi args = do
    evalPrimOp <- State.gets envPrimEval

    case compare (length args) (length tys) of
      LT -> return (VPrim pi args)
      EQ -> evalPrimOp pi args
      GT -> error "applyToPrim: Overapplied prim"
   where
    tys = fst $ splitFunForallTy (primType pi)
{-# SCC evalApplication #-}

-- Bindings in letrec expressions are evaluated on-demand, relying on the
-- behaviour of 'evaluateVar' to prevent cycling.
--
evaluateLetrec :: [LetBinding] -> Term -> Eval Value
evaluateLetrec bs x =
  State.withState addTerms (evaluate x)
 where
  terms = fmap (fmap Left) bs
  addTerms env = foldr (uncurry $ insertEnv LocalId) env terms
{-# SCC evaluateLetrec #-}

evaluateCase :: Term -> Type -> [Alt] -> Eval Value
evaluateCase x ty xs =
  evaluate x >>= \case
    VLit l -> litCase l
    VData dc args -> dataCase dc args
    VPrim pi args -> primCase pi args
    v -> neuCase v
 where
  neuCase s =
    fmap (VNeu . NeCase s ty) (traverse evaluateAlt xs)

  litCase l =
    let eval (pat, e) = case pat of
          LitPat a
            | l == a -> evaluate e
          DefaultPat -> evaluate e
          -- TODO: We hit this now
          _ -> error ("litCase: Cannot match on " <> show pat)

     in evalAlts (const True) eval xs

  evalDataPat args tvs ids e =
    let tys = zip tvs (Either.rights args)
        tms = zip ids (Right <$> Either.lefts args)
        addBinders env = insertAllEnv LocalId tms (insertAllEnvTys tys env)
     in State.withState addBinders (evaluate e)

  dataCase dc args =
    let matches = \case
          DataPat c _ _ -> dc == c
          LitPat _ -> False
          DefaultPat -> True

        eval (pat, e) = case pat of
          DataPat _ tvs ids -> evalDataPat args tvs ids e
          DefaultPat -> evaluate e
          _ -> error ("dataCase: Cannot match on pattern " <> show pat)

     in evalAlts matches eval xs

  primCase _ args =
    let eval (pat, e) = case pat of
          DataPat _ tvs ids -> evalDataPat args tvs ids e
          _ -> evaluate e

     in evalAlts (const True) eval xs
{-# SCC evaluateCase #-}

-- Evalaute an alternative without selecting it.
-- This is used when a case expression is neutral.
--
evaluateAlt :: Alt -> Eval (Pat, Value)
evaluateAlt (pat, x)
  | DataPat _ tvs ids <- pat
  = let tys  = fmap (\tv -> (tv, VarTy tv)) tvs
        tms  = fmap (\i  -> (i, Right . VNeu $ NeVar i)) ids
        addBinders env = insertAllEnv LocalId tms (insertAllEnvTys tys env)
     in (pat,) <$> State.withState addBinders (evaluate x)

  | otherwise
  = (pat,) <$> evaluate x
{-# SCC evaluateAlt #-}

findBestMatch :: (Pat -> Bool) -> [Alt] -> Alt
findBestMatch isMatch alts =
  case filter (isMatch . fst) alts of
    (x:xs) -> bestMatch x xs
    [] -> error "findBestMatch: No matching patterns for case"
 where
  bestMatch (DefaultPat, e) [] = (DefaultPat, e)
  bestMatch (DefaultPat, _) xs = head xs
  bestMatch alt _ = alt

evalAlts :: (Pat -> Bool) -> (Alt -> Eval Value) -> [Alt] -> Eval Value
evalAlts isMatch eval =
  eval . findBestMatch isMatch
{-# SCC evalAlts #-}

evaluateCast :: Term -> Type -> Type -> Eval Value
evaluateCast x a b = do
  evalX <- evaluate x
  return (VCast evalX a b)
{-# SCC evaluateCast #-}

evaluateTick :: Term -> TickInfo -> Eval Value
evaluateTick x ti = do
  evalX <- evaluate x
  return (VTick evalX ti)
{-# SCC evaluateTick #-}

apply :: Value -> Value -> Eval Value
apply (collectValueTicks -> (v1, ts)) v2 =
  case v1 of
    VNeu n -> return (addTicks (VNeu (NeApp n v2)) ts)

    VLam x e env ->
      let addBinder = insertEnv LocalId x (Right v2)
       in fmap (`addTicks` ts) $ State.put env >> State.withState addBinder (evaluate e)

    _ -> error ("apply: Cannot apply value to " <> show v1)
{-# SCC apply #-}

applyTy :: Value -> Type -> Eval Value
applyTy (collectValueTicks -> (v, ts)) ty =
  case v of
    VNeu n ->
      return (addTicks (VNeu (NeTyApp n ty)) ts)

    VTyLam x e env ->
      let addBinder = insertEnvTy x ty
       in fmap (`addTicks` ts) $ State.put env >> State.withState addBinder (evaluate e)

    _ -> error ("applyTy: Cannot apply type to " <> show v)
{-# SCC applyTy #-}

quote :: Value -> Eval Nf
quote = \case
  VData dc args -> quoteData dc args
  VLit l -> return (NLit l)
  VPrim pi args -> quotePrim pi args
  VLam x e env -> quoteLam x e env
  VTyLam x e env -> quoteTyLam x e env
  VCast x a b -> quoteCast x a b
  VTick x ti -> quoteTick x ti
  VNeu n -> NNeu <$> quoteNeutral n
{-# SCC quote #-}

quoteData :: DataCon -> [Either Value Type] -> Eval Nf
quoteData dc args = do
  quoteArgs <- traverse (bitraverse quote return) args
  return (NData dc quoteArgs)
{-# SCC quoteData #-}

quotePrim :: PrimInfo -> [Either Value Type] -> Eval Nf
quotePrim pi args = do
  quoteArgs <- traverse (bitraverse quote return) args
  return (NPrim pi quoteArgs)
{-# SCC quotePrim #-}

quoteLam :: Id -> Term -> Env -> Eval Nf
quoteLam x e env = do
  evalE  <- apply (VLam x e env) (VNeu (NeVar x))
  quoteE <- quote evalE
  return (NLam x quoteE)
{-# SCC quoteLam #-}

quoteTyLam :: TyVar -> Term -> Env -> Eval Nf
quoteTyLam x e env = do
  evalE  <- applyTy (VTyLam x e env) (VarTy x)
  quoteE <- quote evalE
  return (NTyLam x quoteE)
{-# SCC quoteTyLam #-}

quoteCast :: Value -> Type -> Type -> Eval Nf
quoteCast x a b = do
  quoteX <- quote x
  return (NCast quoteX a b)
{-# SCC quoteCast #-}

quoteTick :: Value -> TickInfo -> Eval Nf
quoteTick x ti = do
  quoteX <- quote x
  return (NTick quoteX ti)
{-# SCC quoteTick #-}

quoteNeutral :: Neutral Value -> Eval (Neutral Nf)
quoteNeutral = \case
  NeVar v -> return (NeVar v)
  NeApp x y -> quoteApp x y
  NeTyApp x ty -> quoteTyApp x ty
  NeCase x ty xs -> quoteCase x ty xs
{-# SCC quoteNeutral #-}

quoteApp :: Neutral Value -> Value -> Eval (Neutral Nf)
quoteApp x y = do
  quoteX <- quoteNeutral x
  quoteY <- quote y
  return (NeApp quoteX quoteY)
{-# SCC quoteApp #-}

quoteTyApp :: Neutral Value -> Type -> Eval (Neutral Nf)
quoteTyApp x ty = do
  quoteX <- quoteNeutral x
  return (NeTyApp quoteX ty)
{-# SCC quoteTyApp #-}

quoteCase :: Value -> Type -> [(Pat, Value)] -> Eval (Neutral Nf)
quoteCase x ty xs = do
  quoteX  <- quote x
  quoteXs <- traverse (bitraverse return quote) xs
  return (NeCase quoteX ty quoteXs)
{-# SCC quoteCase #-}

