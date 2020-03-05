{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}

module Clash.GHC.Evaluator.Char
  ( charPrims
  ) where

import Prelude hiding (pi)

import Control.Monad (MonadPlus(mzero))
import Data.Either (lefts)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap (fromList)
import Data.Text (Text)
import GHC.Prim
import GHC.Types

import Clash.Core.Evaluator.Models
import Clash.Core.Term

import Clash.GHC.Evaluator.Convert
import Clash.GHC.Evaluator.Strategy

-- | Primitive Operations defined on Char / Char#
--
charPrims :: HashMap Text EvalPrim
charPrims = HashMap.fromList
  [ ("GHC.Prim.gtChar#", evalComparison# gtChar#)
  , ("GHC.Prim.geChar#", evalComparison# geChar#)
  , ("GHC.Prim.eqChar#", evalComparison# eqChar#)
  , ("GHC.Prim.neChar#", evalComparison# neChar#)
  , ("GHC.Prim.ltChar#", evalComparison# ltChar#)
  , ("GHC.Prim.leChar#", evalComparison# leChar#)
  , ("GHC.Prim.ord#", primOrd)
  , ("GHC.Types.C#", primC)
  ]

primOrd :: EvalPrim
primOrd = evalUnaryOp $ \i ->
  let !(C# a) = i in I# (ord# a)

primC :: EvalPrim
primC pi args
  | [iVal] <- lefts args
  = do !(C# _) <- convItem <$> fromValue iVal
       tyInfo <- typeInfo (primType pi)

       case tyInfo of
         ([], [charDc]) -> return $ VData charDc [Left iVal]
         _ -> mzero

  | otherwise
  = mzero

evalComparison# :: (Char# -> Char# -> Int#) -> EvalPrim
evalComparison# op = evalBinaryOp $ \i j ->
  let !(C# a) = i
      !(C# b) = j
   in I# (a `op` b)

