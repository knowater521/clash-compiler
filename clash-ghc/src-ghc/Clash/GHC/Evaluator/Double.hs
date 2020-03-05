{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnboxedTuples #-}

module Clash.GHC.Evaluator.Double
  ( doublePrims
  ) where

import Prelude hiding (pi)

import Control.Monad (MonadPlus(mzero))
import Data.Either (lefts, rights)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text)
import GHC.Int
import GHC.Prim
import GHC.Types

import Clash.Core.Evaluator.Models
import Clash.Core.Term
import Clash.Core.TysPrim

import Clash.GHC.Evaluator.Convert
import Clash.GHC.Evaluator.Strategy

doublePrims :: HashMap Text EvalPrim
doublePrims = HashMap.fromList
  [ ("GHC.Prim.>##", evalComparison# (>##))
  , ("GHC.Prim.>=##", evalComparison# (>=##))
  , ("GHC.Prim.==##", evalComparison# (==##))
  , ("GHC.Prim./=##", evalComparison# (/=##))
  , ("GHC.Prim.<##", evalComparison# (<##))
  , ("GHC.Prim.<=##", evalComparison# (<=##))
  , ("GHC.Prim.+##", evalBinaryOp# (+##))
  , ("GHC.Prim.-##", evalBinaryOp# (-##))
  , ("GHC.Prim.*##", evalBinaryOp# (*##))
  , ("GHC.Prim./##", evalBinaryOp# (/##))
  , ("GHC.Prim.negateDouble#", evalUnaryOp# negateDouble#)
  , ("GHC.Prim.fabsDouble#", evalUnaryOp# fabsDouble#)
  , ("GHC.Prim.double2Int#", primDouble2Int)
  , ("GHC.Prim.double2Float#", primDouble2Float)
  , ("GHC.Prim.expDouble#", evalUnaryOp# expDouble#)
  , ("GHC.Prim.logDouble#", evalUnaryOp# logDouble#)
  , ("GHC.Prim.sqrtDouble#", evalUnaryOp# sqrtDouble#)
  , ("GHC.Prim.sinDouble#", evalUnaryOp# sinDouble#)
  , ("GHC.Prim.cosDouble#", evalUnaryOp# cosDouble#)
  , ("GHC.Prim.tanDouble#", evalUnaryOp# tanDouble#)
  , ("GHC.Prim.asinDouble#", evalUnaryOp# asinDouble#)
  , ("GHC.Prim.acosDouble#", evalUnaryOp# acosDouble#)
  , ("GHC.Prim.atanDouble#", evalUnaryOp# atanDouble#)
  , ("GHC.Prim.sinhDouble#", evalUnaryOp# sinhDouble#)
  , ("GHC.Prim.coshDouble#", evalUnaryOp# coshDouble#)
  , ("GHC.Prim.tanhDouble#", evalUnaryOp# tanhDouble#)

#if MIN_VERSION_ghc(8,7,0)
  , ("GHC.Prim.asinhDouble#", evalUnaryOp# asinhDouble#)
  , ("GHC.Prim.acoshDouble#", evalUnaryOp# acoshDouble#)
  , ("GHC.Prim.atanhDouble#", evalUnaryOp# atanhDouble#)
#endif

#if MIN_VERSION_base(4,12,0)
  , ("GHC.Float.$w$casinh", primAsinhSpecialized)
#endif

  , ("GHC.Prim.**##", evalBinaryOp# (**##))
  , ("GHC.Prim.decodeDouble_2Int#", primDecodeDouble2Int)
  , ("GHC.Prim.decodeDouble_Int64#", primDecodeDoubleInt64)
  ]

primDouble2Int :: EvalPrim
primDouble2Int = evalUnaryOp $ \i -> 
  let !(D# a) = i in I# (double2Int# a)

primDouble2Float :: EvalPrim
primDouble2Float = evalUnaryOp $ \i ->
  let !(D# a) = i in F# (double2Float# a)

#if MIN_VERSION_base(4,12,0)
primAsinhSpecialized :: EvalPrim
primAsinhSpecialized = evalUnaryOp# $ \i ->
  let !(D# a) = asinh (D# i) in a
#endif

primDecodeDouble2Int :: EvalPrim
primDecodeDouble2Int pi args
  | [iVal] <- lefts args
  = do !(D# a) <- convItem <$> fromValue iVal
       resTy <- resultType (primType pi) (rights args)
       let !(# p, q, r, s #) = decodeDouble_2Int# a

       let res = Converted (I# p, W# q, W# r, I# s)
                   (intPrimTy, wordPrimTy, wordPrimTy, intPrimTy, (), (), (), ())
       toValue (res, resTy)

  | otherwise
  = mzero

primDecodeDoubleInt64 :: EvalPrim 
primDecodeDoubleInt64 pi args
  | [iVal] <- lefts args
  = do !(D# a) <- convItem <$> fromValue iVal
       resTy <- resultType (primType pi) (rights args)
       let !(# p, q #) = decodeDouble_Int64# a

       let res = Converted (I64# p, I# q) (intPrimTy, intPrimTy, (), ())
       toValue (res, resTy)

  | otherwise
  = mzero

evalUnaryOp# :: (Double# -> Double#) -> EvalPrim
evalUnaryOp# op = evalUnaryOp $ \i ->
  let !(D# a) = i in D# (op a)

evalBinaryOp# :: (Double# -> Double# -> Double#) -> EvalPrim
evalBinaryOp# op = evalBinaryOp $ \i j ->
  let !(D# a) = i
      !(D# b) = j
   in D# (a `op` b)

evalComparison# :: (Double# -> Double# -> Int#) -> EvalPrim
evalComparison# op = evalBinaryOp $ \i j ->
  let !(D# a) = i
      !(D# b) = j
   in I# (a `op` b)

