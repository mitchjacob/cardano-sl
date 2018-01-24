{-# LANGUAGE RankNTypes #-}

-- | Helper functions related to delegation.

module Pos.Delegation.Helpers
       ( dlgVerifyPayload
       , isRevokePsk
       , dlgMemPoolApplyBlock
       ) where

import           Universum

import           Control.Lens ((%=))
import           Control.Monad.Except (MonadError (throwError))
import qualified Data.HashMap.Strict as HM
import           Data.List (partition)

import           Pos.Core (EpochIndex)
import           Pos.Core.Block.Main (MainBlock, mainBlockDlgPayload)
import           Pos.Core.Delegation.Types (DlgMemPool, DlgPayload (getDlgPayload))
import           Pos.Crypto (ProxySecretKey (..), isSelfSignedPsk)

-- | Checks if given PSK revokes delegation (issuer == delegate).
isRevokePsk :: ProxySecretKey w -> Bool
isRevokePsk = isSelfSignedPsk

-- | Applies block certificates to 'ProxySKHeavyMap'.
dlgMemPoolApplyBlock :: MainBlock -> DlgMemPool -> DlgMemPool
dlgMemPoolApplyBlock block m = flip execState m $ do
    let (toDelete,toReplace) =
            partition isRevokePsk (getDlgPayload $ block ^. mainBlockDlgPayload)
    for_ toDelete $ \psk -> identity %= HM.delete (pskIssuerPk psk)
    for_ toReplace $ \psk -> identity %= HM.insert (pskIssuerPk psk) psk
