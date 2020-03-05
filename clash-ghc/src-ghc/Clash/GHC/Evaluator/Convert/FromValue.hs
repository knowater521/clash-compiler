{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise       #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Extra.Solver    #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Clash.GHC.Evaluator.Convert.FromValue
  ( FromValue(..)
  ) where

import Prelude hiding (pi)

import Control.Monad.Trans.Maybe (MaybeT)
import Data.Bits ((.&.))
import Data.Constraint
import Data.Either (lefts)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Primitive.ByteArray (ByteArray(..))
import Data.Proxy
import Data.Word (Word8, Word16, Word32, Word64)
import GHC.Integer.GMP.Internals
import GHC.Natural
import GHC.TypeLits.KnownNat
import GHC.TypeNats
import Unsafe.Coerce

import Clash.Sized.Internal.BitVector
import Clash.Sized.Internal.Index
import Clash.Sized.Internal.Signed
import Clash.Sized.Internal.Unsigned
import Clash.Sized.Vector

import Clash.Core.DataCon (dcTag)
import Clash.Core.Evaluator.Models
import Clash.Core.Literal
import Clash.Core.Term (PrimInfo(primName))

import Clash.GHC.Evaluator.Convert.Internal

class FromValue a where
  fromValue :: Value -> MaybeT Eval (Converted a)

-- Haskell Types

instance FromValue ByteArray where
  fromValue = \case
    VLit (ByteArrayLiteral ba) -> convert ba ()
    _ -> noConversion

instance FromValue Char where
  fromValue = \case
    VLit (CharLiteral c) -> convert c ()
    _ -> noConversion

instance FromValue Double where
  fromValue = \case
    VLit (DoubleLiteral r) -> convert (fromRational r) ()
    _ -> noConversion

instance FromValue Float where
  fromValue = \case
    VLit (FloatLiteral r) -> convert (fromRational r) ()
    _ -> noConversion

instance FromValue Int where
  fromValue = \case
    VLit (IntLiteral x) -> convert (fromInteger x) ()
    _ -> noConversion

instance FromValue Int8 where
  fromValue = \case
    VLit (IntLiteral x) -> convert (fromInteger x) ()
    _ -> noConversion

instance FromValue Int16 where
  fromValue = \case
    VLit (IntLiteral x) -> convert (fromInteger x) ()
    _ -> noConversion

instance FromValue Int32 where
  fromValue = \case
    VLit (IntLiteral x) -> convert (fromInteger x) ()
    _ -> noConversion

instance FromValue Int64 where
  fromValue = \case
    VLit (IntLiteral x) -> convert (fromInteger x) ()
    _ -> noConversion

instance FromValue Integer where
  fromValue = \case
    VLit (IntegerLiteral x) -> convert x ()
    VData dc args ->
      case (dcTag dc, lefts args) of
        (1, [v]) -> do
          i <- convItem <$> fromValue @Int v
          convert (toInteger i) ()

        (2, [v]) -> do
          ByteArray ba <- convItem <$> fromValue v
          convert (Jp# (BN# ba)) ()

        (3, [v]) -> do
          ByteArray ba <- convItem <$> fromValue v
          convert (Jn# (BN# ba)) ()

        _ -> noConversion

    _ -> noConversion

instance FromValue Natural where
  fromValue = \case
    VLit (NaturalLiteral x) -> convert (fromInteger x) ()
    VData dc args ->
      case (dcTag dc, lefts args) of
        (1, [v]) -> do
          i <- convItem <$> fromValue @Word v
          convert (wordToNatural i) ()

        (2, [v]) -> do
          ByteArray ba <- convItem <$> fromValue v
          convert (NatJ# (BN# ba)) ()

        _ -> noConversion

    _ -> noConversion

instance FromValue Word where
  fromValue = \case
    VLit (WordLiteral x) -> convert (fromInteger x) ()
    _ -> noConversion

instance FromValue Word8 where
  fromValue = \case
    VLit (WordLiteral x) -> convert (fromInteger x) ()
    _ -> noConversion

instance FromValue Word16 where
  fromValue = \case
    VLit (WordLiteral x) -> convert (fromInteger x) ()
    _ -> noConversion

instance FromValue Word32 where
  fromValue = \case
    VLit (WordLiteral x) -> convert (fromInteger x) ()
    _ -> noConversion

instance FromValue Word64 where
  fromValue = \case
    VLit (WordLiteral x) -> convert (fromInteger x) ()
    _ -> noConversion

-- Clash Types

instance FromValue Bit where
  fromValue = \case
    VPrim pi [Left mVal, Left iVal]
      |  primName pi == "Clash.Sized.Internal.BitVector.fromInteger##"
      -> do m <- convItem <$> fromValue mVal
            i <- convItem <$> fromValue iVal
            convert (Bit (m .&. 1) (i .&. 1)) ()

    _ -> noConversion

instance FromValue (BitVector n) where
  fromValue = \case
    VPrim pi [Right nTy, Left knownN, Left mVal, Left iVal]
      |  primName pi == "Clash.Sized.Internal.BitVector.fromInteger#"
      ,  VLit (NaturalLiteral nat) <- knownN
      -> do m <- convItem <$> fromValue mVal
            i <- convItem <$> fromValue iVal
            convert (BV m i) (nTy, naturalFromInteger nat)

    _ -> noConversion

instance FromValue (Index n) where
  fromValue = \case
    VPrim pi [Right nTy, Left knownN, Left iVal]
      |  primName pi == "Clash.Sized.Internal.Index.fromInteger#"
      ,  VLit (NaturalLiteral nat) <- knownN
      -> do i <- convItem <$> fromValue iVal
            convert (I i) (nTy, naturalFromInteger nat)

    _ -> noConversion

instance FromValue (Signed n) where
  fromValue = \case
    VPrim pi [Right nTy, Left knownN, Left iVal]
      |  primName pi == "Clash.Sized.Internal.Signed.fromInteger#"
      ,  VLit (NaturalLiteral nat) <- knownN
      -> do i <- convItem <$> fromValue iVal
            convert (S i) (nTy, naturalFromInteger nat)

    _ -> noConversion

instance FromValue (Unsigned n) where
  fromValue = \case
    VPrim pi [Right nTy, Left knownN, Left iVal]
      |  primName pi == "Clash.Sized.Internal.Unsigned.fromInteger#"
      ,  VLit (NaturalLiteral nat) <- knownN
      -> do i <- convItem <$> fromValue iVal
            convert (U i) (nTy, naturalFromInteger nat)

    _ -> noConversion

-- This instance is truly horiffic, as it
--
--   * needs to know the length of the vector
--   * can only call fromNil if it is known to be == 0
--   * can only call fromCons if it is known to be >= 1
--
-- In lieu of a better method, we solve this by using the constraints package
-- to forcably rewrite (1 <=? m) ~ 'False to m ~ 0. It would be nice if this
-- was handled somewhere else (e.g. ghc-typelits-natnormalize, which has the
-- KnownBool class).
--
instance (KnownNat n, KnownBool (1 <=? n), FromValue a) => FromValue (Vec n a) where
  fromValue v = case boolSing @(1 <=? n) of
    SFalse -> fromNil v \\ nleOneIsZero @n
    STrue  -> fromCons v
   where
    -- Annoyingly, we need to make this constraint, as neither GHC nor the
    -- type checker plugins used can figure it out.
    --
    nleOneIsZero :: forall m. ((1 <=? m) ~ 'False) :- (m ~ 0)
    nleOneIsZero = Sub (unsafeCoerce (Dict :: Dict (m ~ m)))

    fromNil :: Value -> MaybeT Eval (Converted (Vec 0 a))
    fromNil = \case
      VData _ [Right nTy, Right aTy, Left co] ->
        let va = VecArgs (nTy, 0) aTy Nil co Nil
         in convert Nil va

      _ -> noConversion

    fromCons
      :: forall m. (KnownNat m, KnownNat (m - 1), 1 <= m)
      => Value -> MaybeT Eval (Converted (Vec m a))
    fromCons = \case
      VData _ [Right nTy, Right aTy, Right _, Left co, Left xVal, Left xsVal] -> do
        Converted x xArgs <- fromValue @a xVal
        Converted xs xsArgs <- fromValue @(Vec (m - 1) a) xsVal

        let kn    = (nTy, natVal (Proxy @m))
            eArgs = Cons xArgs (vaElemArgs xsArgs)
            eCos  = Cons co (vaElemCos xsArgs)
            va    = VecArgs kn aTy eArgs (vaNilCo xsArgs) eCos
         in convert (Cons x xs) va

      _ -> noConversion

