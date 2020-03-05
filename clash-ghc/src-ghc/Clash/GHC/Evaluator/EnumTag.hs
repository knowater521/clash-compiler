{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Clash.GHC.Evaluator.EnumTag
  ( enumTagPrims
  ) where

import Control.Monad (MonadPlus(mzero))
import qualified Control.Monad.State.Strict as State
import Control.Monad.Trans.Maybe (MaybeT(..))
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.List (find)
import Data.Text (Text)

import Clash.Core.DataCon
import Clash.Core.Evaluator.Models
import Clash.Core.TyCon
import Clash.Core.Type
import Clash.Unique

import Clash.GHC.Evaluator.Convert

enumTagPrims :: HashMap Text EvalPrim
enumTagPrims = HashMap.fromList
  [ ("GHC.Prim.tagToEnum#", primTagToEnum)
  ]

primTagToEnum :: EvalPrim
primTagToEnum _ = \case
  [Right (ConstTy (TyCon tcN)), Left iVal] -> do
    tcm <- State.gets envTcMap
    i   <- convItem <$> fromValue iVal
    tc  <- hoist $ lookupUniqMap tcN tcm
    dc  <- hoist $ find (\x -> dcTag x == i + 1) (tyConDataCons tc)

    return $ VData dc []

  _ -> mzero
 where
  hoist = MaybeT . return

