{-# LANGUAGE RankNTypes #-}

-- | Helper functions related to delegation.

module Pos.Delegation.Helpers
       ( isRevokePsk
       , dlgVerifyPayload
       ) where

import           Universum

import           Control.Monad.Except (MonadError (throwError))
import qualified Data.Set as S

import           Pos.Core.Delegation (DlgPayload (getDlgPayload), HeavyDlgIndex (..))
import           Pos.Core.Slotting (EpochIndex)
import           Pos.Crypto (ProxySecretKey (..), isSelfSignedPsk)

-- | Checks if given PSK revokes delegation (issuer == delegate).
isRevokePsk :: ProxySecretKey w -> Bool
isRevokePsk = isSelfSignedPsk

-- | Verify delegation payload without using GState. This function can
-- be used for block verification in isolation, also it can be used
-- for mempool verification.
dlgVerifyPayload :: MonadError Text m => EpochIndex -> DlgPayload -> m ()
dlgVerifyPayload epoch (getDlgPayload -> proxySKs) =
    unless (S.null notMatchingEpochs) $
    throwError "Block contains psk(s) that have non-matching epoch index"
  where
    notMatchingEpochs = S.filter ((/= epoch) . getHeavyDlgIndex . pskOmega) proxySKs
