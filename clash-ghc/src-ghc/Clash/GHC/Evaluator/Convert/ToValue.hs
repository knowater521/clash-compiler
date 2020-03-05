{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Clash.GHC.Evaluator.Convert.ToValue
  ( ToValue(..)
  ) where

import Control.Monad (MonadPlus(mzero), foldM)
import qualified Control.Monad.State.Strict as State
import Control.Monad.Trans.Maybe (MaybeT)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.List (zip4)
import Data.Primitive.ByteArray (ByteArray)
import Data.Word (Word8, Word16, Word32, Word64)
import GHC.TypeLits (KnownNat)
import Numeric.Natural

import Clash.Sized.Internal.BitVector (Bit(..), BitVector(..))
import Clash.Sized.Internal.Index (Index(..))
import Clash.Sized.Internal.Signed (Signed(..))
import Clash.Sized.Internal.Unsigned (Unsigned(..))
import Clash.Sized.Vector (Vec(..))
import qualified Clash.Sized.Vector as Vec (toList)

import Clash.Core.DataCon (DataCon(dcType))
import Clash.Core.Evaluator.Models
import Clash.Core.Literal
import Clash.Core.Term
import Clash.Core.Type
import Clash.Core.TysPrim

import Clash.GHC.Evaluator.Common
import Clash.GHC.Evaluator.Convert.Internal

dataConsOf :: Type -> MaybeT Eval [DataCon]
dataConsOf ty = do
  tcm <- State.gets envTcMap
  return $ snd (typeInfo tcm ty)

class ToValue a where
  toValue :: (Converted a, Type) -> MaybeT Eval Value

-- Container Types

instance (ToValue a) => ToValue [a] where
  toValue (Converted xs (aTy, aArgs), ty) = do
    [nilDc, consDc] <- dataConsOf ty
    foldM (mkCons consDc) (mkNil nilDc) xs
   where
    mkNil dc =
      VData dc [Right aTy]

    mkCons dc acc x = do
      xVal <- toValue (Converted x aArgs, aTy)
      return $ VData dc [Right aTy, Left xVal, Left acc]

instance (ToValue a, ToValue b) => ToValue (a, b) where
  toValue (Converted (a, b) (aTy, bTy, aArgs, bArgs), ty) = do
    [tupDc] <- dataConsOf ty
    aVal <- toValue (Converted a aArgs, aTy)
    bVal <- toValue (Converted b bArgs, bTy)

    return $ VData tupDc [Right aTy, Right bTy, Left aVal, Left bVal]

instance (ToValue a, ToValue b, ToValue c) => ToValue (a, b, c) where
  toValue (Converted (a, b, c) (aTy, bTy, cTy, aArgs, bArgs, cArgs), ty) = do
    [tupDc] <- dataConsOf ty
    aVal <- toValue (Converted a aArgs, aTy)
    bVal <- toValue (Converted b bArgs, bTy)
    cVal <- toValue (Converted c cArgs, cTy)

    return $ VData tupDc
      [Right aTy, Right bTy, Right cTy, Left aVal, Left bVal, Left cVal]

instance (ToValue a, ToValue b, ToValue c, ToValue d) => ToValue (a, b, c, d) where
  toValue (Converted (a, b, c, d) (aTy, bTy, cTy, dTy, aArgs, bArgs, cArgs, dArgs), ty) = do
    [tupDc] <- dataConsOf ty
    aVal <- toValue (Converted a aArgs, aTy)
    bVal <- toValue (Converted b bArgs, bTy)
    cVal <- toValue (Converted c cArgs, cTy)
    dVal <- toValue (Converted d dArgs, dTy)

    return $ VData tupDc
      [Right aTy, Right bTy, Right cTy, Right dTy, Left aVal, Left bVal, Left cVal, Left dVal]

instance (ToValue a, ToValue b) => ToValue (Either a b) where
  toValue (Converted eab (aTy, bTy, eabArgs), ty) = do
    [leftDc, rightDc] <- dataConsOf ty

    case (eab, eabArgs) of
      (Left a, Left aArgs) -> do
        aVal <- toValue (Converted a aArgs, aTy)
        return $ VData leftDc [Right aTy, Right bTy, Left aVal]

      (Right b, Right bArgs) -> do
        bVal <- toValue (Converted b bArgs, bTy)
        return $ VData rightDc [Right bTy, Right aTy, Left bVal]

      _ -> mzero

-- Haskell Types

instance ToValue Bool where
  toValue (Converted x (), ty) = do
    [falseDc, trueDc] <- dataConsOf ty
    return $ VData (if x then trueDc else falseDc) []

instance ToValue ByteArray where
  toValue (Converted x (), _) =
    return $ VLit (ByteArrayLiteral x)

instance ToValue Char where
  toValue (Converted x (), _) =
    return $ VLit (CharLiteral x)

instance ToValue Double where
  toValue (Converted x (), _) =
    return $ VLit (DoubleLiteral $ toRational x)

instance ToValue Float where
  toValue (Converted x (), _) =
    return $ VLit (FloatLiteral $ toRational x)

instance ToValue Int where
  toValue (Converted x (), _) =
    return $ VLit (IntLiteral $ toInteger x)

instance ToValue Int8 where
  toValue (Converted x (), _) =
    return $ VLit (IntLiteral $ toInteger x)

instance ToValue Int16 where
  toValue (Converted x (), _) =
    return $ VLit (IntLiteral $ toInteger x)

instance ToValue Int32 where
  toValue (Converted x (), _) =
    return $ VLit (IntLiteral $ toInteger x)

instance ToValue Int64 where
  toValue (Converted x (), _) =
    return $ VLit (IntLiteral $ toInteger x)

instance ToValue Integer where
  toValue (Converted x (), _) =
    return $ VLit (IntegerLiteral x)

instance ToValue Natural where
  toValue (Converted x (), _) =
    return $ VLit (NaturalLiteral $ toInteger x)

instance ToValue Ordering where
  toValue (Converted x (), ty) = do
    [ltDc, eqDc, gtDc] <- dataConsOf ty

    return $ case x of
      LT -> VData ltDc []
      EQ -> VData eqDc []
      GT -> VData gtDc []

instance ToValue Word where
  toValue (Converted x (), _) =
    return $ VLit (WordLiteral $ toInteger x)

instance ToValue Word8 where
  toValue (Converted x (), _) =
    return $ VLit (WordLiteral $ toInteger x)

instance ToValue Word16 where
  toValue (Converted x (), _) =
    return $ VLit (WordLiteral $ toInteger x)

instance ToValue Word32 where
  toValue (Converted x (), _) =
    return $ VLit (WordLiteral $ toInteger x)

instance ToValue Word64 where
  toValue (Converted x (), _) =
    return $ VLit (WordLiteral $ toInteger x)

-- Clash Types

instance ToValue Bit where
  toValue (Converted (Bit m i) (), ty) = do
    [bitDc] <- dataConsOf ty
    mVal <- toValue (Converted m (), integerPrimTy)
    iVal <- toValue (Converted i (), integerPrimTy)

    return $ VPrim (mkPrimInfo bitDc) [Left mVal, Left iVal]
   where
    mkPrimInfo dc = PrimInfo
      { primName = "Clash.Sized.Internal.BitVector.fromInteger##"
      , primType = dcType dc
      , primWorkInfo = WorkNever
      }

instance (KnownNat n) => ToValue (BitVector n) where
  toValue (Converted (BV m i) (nTy, kn), ty) = do
    [bvDc] <- dataConsOf ty

    let knVal = VLit (NaturalLiteral $ toInteger kn)
    mVal <- toValue (Converted m (), integerPrimTy)
    iVal <- toValue (Converted i (), integerPrimTy)

    case nTy of
      VarTy nTv -> return $ VPrim (mkPrimInfo bvDc nTv)
        [Right nTy, Left knVal, Left mVal, Left iVal]

      _ -> mzero
   where
    mkPrimInfo dc tv = PrimInfo
      { primName = "Clash.Sized.Internal.BitVector.fromInteger#"
      , primType = ForAllTy tv (AppTy (AppTy (ConstTy Arrow) naturalPrimTy) (dcType dc))
      , primWorkInfo = WorkNever
      }

instance (KnownNat n) => ToValue (Index n) where
  toValue (Converted (I i) (nTy, kn), ty) = do
    [indexDc] <- dataConsOf ty

    let knVal = VLit (NaturalLiteral $ toInteger kn)
    iVal <- toValue (Converted i (), integerPrimTy)

    case nTy of
      VarTy nTv -> return $ VPrim (mkPrimInfo indexDc nTv)
        [Right nTy, Left knVal, Left iVal]

      _ -> mzero
   where
    mkPrimInfo dc tv = PrimInfo
      { primName = "Clash.Sized.Internal.Index.fromInteger#"
      , primType = ForAllTy tv (AppTy (AppTy (ConstTy Arrow) naturalPrimTy) (dcType dc))
      , primWorkInfo = WorkNever
      }

instance (KnownNat n) => ToValue (Signed n) where
  toValue (Converted (S i) (nTy, kn), ty) = do
    [signedDc] <- dataConsOf ty

    let knVal = VLit (NaturalLiteral $ toInteger kn)
    iVal <- toValue (Converted i (), integerPrimTy)

    case nTy of
      VarTy nTv -> return $ VPrim (mkPrimInfo signedDc nTv)
        [Right nTy, Left knVal, Left iVal]

      _ -> mzero
   where
    mkPrimInfo dc tv = PrimInfo
      { primName = "Clash.Sized.Internal.Signed.fromInteger#"
      , primType = ForAllTy tv (AppTy (AppTy (ConstTy Arrow) naturalPrimTy) (dcType dc))
      , primWorkInfo = WorkNever
      }

instance (KnownNat n) => ToValue (Unsigned n) where
  toValue (Converted (U i) (nTy, kn), ty) = do
    [unsignedDc] <- dataConsOf ty

    let knVal = VLit (NaturalLiteral $ toInteger kn)
    iVal <- toValue (Converted i (), integerPrimTy)

    case nTy of
      VarTy nTv -> return $ VPrim (mkPrimInfo unsignedDc nTv)
        [Right nTy, Left knVal, Left iVal]

      _ -> mzero
   where
    mkPrimInfo dc tv = PrimInfo
      { primName = "Clash.Sized.Internal.Unsigned.fromInteger#"
      , primType = ForAllTy tv (AppTy (AppTy (ConstTy Arrow) naturalPrimTy) (dcType dc))
      , primWorkInfo = WorkNever
      }

instance (ToValue a) => ToValue (Vec n a) where
  toValue (Converted xs va, ty) = do
    [nilDc, consDc] <- dataConsOf ty
    
    let nats  = [1..(snd (vaKnownNat va))]
        args  = Vec.toList (vaElemArgs va)
        coes  = Vec.toList (vaElemCos va)
        elems = zip4 nats (Vec.toList xs) args coes
     in foldM (mkCons consDc) (mkNil nilDc) elems
   where
    mkNil dc =
      VData dc [Right (LitTy (NumTy 0)), Right (vaElemTy va), Left (vaNilCo va)]

    mkCons dc acc (n, x, args, co) = do
      xVal <- toValue (Converted x args, vaElemTy va)
      let i = toInteger n

      return $ VData dc
        [ Right (LitTy (NumTy i))
        , Right (vaElemTy va)
        , Right (LitTy (NumTy (i - 1)))
        , Left co
        , Left xVal
        , Left acc
        ]

