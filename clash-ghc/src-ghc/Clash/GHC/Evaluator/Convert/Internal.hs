{-# LANGUAGE TypeFamilies #-}

module Clash.GHC.Evaluator.Convert.Internal
  ( Converted(..)
  , ConvArgs
  , KnownNatArg
  , VecArgs(..)
  , noConversion
  , convert
  ) where

import Control.Monad (MonadPlus(mzero))
import Control.Monad.Trans.Maybe (MaybeT(..))
import Numeric.Natural (Natural)

import Clash.Sized.Internal.BitVector (BitVector)
import Clash.Sized.Internal.Index (Index)
import Clash.Sized.Internal.Signed (Signed)
import Clash.Sized.Internal.Unsigned (Unsigned)
import Clash.Sized.Vector (Vec)

import Clash.Core.Evaluator.Models (Eval, Value)
import Clash.Core.Type (Type)

-- | A KnownNat constraint adds a type of kind Nat, and a value corresponding
-- to the KnownNat dictionary (which in reality is just the chosen number).
--
type KnownNatArg = (Type, Natural)

-- | Vectors have additional coercions for each element which we want to keep,
-- as recreating these coercions may be costly / impossible from the evaluator.
-- Even if Clash discards a coercion, it's type is still needed and would have
-- to be recreated (which may be done incorrectly).
-- 
data VecArgs n a = VecArgs
  { vaKnownNat :: KnownNatArg
  , vaElemTy   :: Type
  , vaElemArgs :: Vec n (ConvArgs a)
  , vaNilCo    :: Value
  , vaElemCos  :: Vec n Value
  }

-- | Different data types may have different additional arguments which are
-- needed when converting back to values. Rather than discarding and attempting
-- to recreate these values, we keep them in the conversion result.
--
-- To support arbitrary nesting of types without confusion as to which
-- arguments belong where, arguments are structured in a way which mirrors the
-- type under conversion.
--
type family ConvArgs a where
  ConvArgs [a]           = (Type, ConvArgs a)
  ConvArgs (a, b)        = (Type, Type, ConvArgs a, ConvArgs b)
  ConvArgs (a, b, c)     = (Type, Type, Type, ConvArgs a, ConvArgs b, ConvArgs c)
  ConvArgs (a, b, c, d)  = (Type, Type, Type, Type, ConvArgs a, ConvArgs b, ConvArgs c, ConvArgs d)
  ConvArgs (Either a b)  = (Type, Type, Either (ConvArgs a) (ConvArgs b))
  ConvArgs (BitVector n) = KnownNatArg
  ConvArgs (Index n)     = KnownNatArg
  ConvArgs (Signed n)    = KnownNatArg
  ConvArgs (Unsigned n)  = KnownNatArg
  ConvArgs (Vec n a)     = VecArgs n a
  ConvArgs _             = ()

-- | The result of converting from a Value to a term is not only the term, but
-- also additional arguments.
data Converted a = Converted
  { convItem :: a
  , convArgs :: ConvArgs a
  }

-- | When defining conversions to/from Value for a type, it may not be
-- possible to convert every possible input (i.e. there may be invariants).
-- If an invariant is not satisfied, this value is used instead to signal to
-- readers that no conversion is possible.
--
noConversion :: MaybeT Eval a
noConversion = mzero

convert :: a -> ConvArgs a -> MaybeT Eval (Converted a)
convert x args = return (Converted x args)

