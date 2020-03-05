{-# LANGUAGE TypeFamilies #-}

module Clash.GHC.Evaluator.Strategy where

import Prelude hiding (pi)

import Control.Monad (MonadPlus(mzero))
import qualified Control.Monad.State.Strict as State
import Control.Monad.Trans.Maybe (MaybeT(..))
import Data.Either (lefts, rights)
import Debug.Trace (traceM)

import Clash.Core.DataCon (DataCon)
import Clash.Core.Evaluator.Models
import Clash.Core.Term (PrimInfo(..))
import Clash.Core.TyCon (tyConDataCons)
import Clash.Core.Type (Type, TypeView(..), splitFunForallTy, tyView)
import Clash.Core.Util (piResultTys)
import Clash.Unique (lookupUniqMap)

import Clash.GHC.Evaluator.Convert

-- | For primitives which are themselves values, we return Nothing.
-- The semantics of the partial evaluator mean that this will make the Prim
-- into a Neutral prim, so no attempt to evaluate it again will occur.
--
evalId :: EvalPrim
evalId _ _ = mzero

-- | For primitives which are known to exist but are not currently implemented.
-- TODO: This should be hidden behind a check for DEBUG
evalMissing :: EvalPrim
evalMissing pi args = do
  traceM ("No implementation for prim: " <> show (primName pi))
  traceM ("Using arguments: " <> show args)
  mzero

-- Evaluate a generic unary op, provided a means to extract a literal
-- from the input value, and convert the output to a value.
--
evalUnaryOp
  :: (FromValue a, ToValue b, ConvArgs a ~ (), ConvArgs b ~ ())
  => (a -> b)
  -> EvalPrim
evalUnaryOp op pi args
  | [xVal] <- lefts args
  = do x <- convItem <$> fromValue xVal
       resTy <- resultType (primType pi) (rights args)

       toValue (Converted (op x) (), resTy)

  | otherwise
  = mzero

-- Evaluate a generic binary op, provided a means to extract literals
-- from the input values, and convert the output to a value.
--
evalBinaryOp
  :: ( FromValue a
     , FromValue b
     , ToValue c
     , ConvArgs a ~ ()
     , ConvArgs b ~ ()
     , ConvArgs c ~ ()
     )
  => (a -> b -> c)
  -> EvalPrim
evalBinaryOp op pi args
  | [xVal, yVal] <- lefts args
  = do x <- convItem <$> fromValue xVal
       y <- convItem <$> fromValue yVal
       resTy <- resultType (primType pi) (rights args)

       toValue (Converted (op x y) (), resTy)

  | otherwise
  = mzero

typeInfo :: Type -> MaybeT Eval ([Type], [DataCon])
typeInfo ty = do
  tcm <- State.gets envTcMap

  case tyView $ snd (splitFunForallTy ty) of
    TyConApp tcNm tyArgs -> do
      tc <- MaybeT $ return (lookupUniqMap tcNm tcm)
      return (tyArgs, tyConDataCons tc)

    _ -> mzero

resultType :: Type -> [Type] -> MaybeT Eval Type
resultType ty tys = do
  tcm <- State.gets envTcMap
  return . snd $ splitFunForallTy (piResultTys tcm ty tys)

