{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Clash.GHC.Evaluator.Index
  ( indexPrims
  ) where

import Prelude hiding (pi)

import Data.Either (lefts, rights)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text)
import GHC.TypeLits (KnownNat)

import Clash.Sized.Internal.Index

import Clash.Core.Evaluator.Models
import Clash.Core.Term (PrimInfo(..))

import Clash.GHC.Evaluator.Convert
import Clash.GHC.Evaluator.Strategy


indexPrims :: HashMap Text EvalPrim
indexPrims = HashMap.fromList
  [ ("Clash.Sized.Internal.Index.fromInteger#", evalId)

    -- BitPack
  , ("Clash.Sized.Internal.Index.pack#", evalMissing)
  , ("Clash.Sized.Internal.Index.unpack#", evalMissing)

    -- Eq
  , ("Clash.Sized.Internal.Index.eq#", evalComparison eq#)
  , ("Clash.Sized.Internal.Index.neq#", evalComparison neq#)

    -- Ord
  , ("Clash.Sized.Internal.Index.lt#", evalComparison lt#)
  , ("Clash.Sized.Internal.Index.le#", evalComparison le#)
  , ("Clash.Sized.Internal.Index.gt#", evalComparison gt#)
  , ("Clash.Sized.Internal.Index.ge#", evalComparison ge#)

    -- Bounded
  , ("Clash.Sized.Internal.Index.maxBound#", evalMissing)

    -- Num
  , ("Clash.Sized.Internal.Index.+#", evalSimpleNum (+#))
  , ("Clash.Sized.Internal.Index.-#", evalSimpleNum (-#))
  , ("Clash.Sized.Internal.Index.*#", evalSimpleNum (*#))

    -- ExtendingNum
  , ("Clash.Sized.Internal.Index.plus#", evalMissing)
  , ("Clash.Sized.Internal.Index.minus#", evalMissing)
  , ("Clash.Sized.Internal.Index.times#", evalMissing)

    -- Integral
  , ("Clash.Sized.Internal.Index.quot#", evalMissing)
  , ("Clash.Sized.Internal.Index.rem#", evalMissing)
  , ("Clash.Sized.Internal.Index.toInteger#", evalMissing)

    -- Resize
  , ("Clash.Sized.Internal.Index.resize#", evalMissing)
  ]

evalSimpleNum :: forall n. (KnownNat n) => (Index n -> Index n -> Index n) -> EvalPrim
evalSimpleNum op pi args
  | [xVal, yVal] <- lefts args
  = do Converted x xArgs <- fromValue xVal
       y <- convItem <$> fromValue yVal
       resTy <- resultType (primType pi) (rights args)

       toValue (Converted (op x y) xArgs, resTy)

  | otherwise
  = evalId pi args

evalComparison :: (Index n -> Index n -> Bool) -> EvalPrim
evalComparison op pi args
  | [xVal, yVal] <- lefts args
  = do x <- convItem <$> fromValue xVal
       y <- convItem <$> fromValue yVal
       resTy <- resultType (primType pi) (rights args)

       toValue (Converted (op x y) (), resTy)

  | otherwise
  = evalId pi args

