{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE RankNTypes #-}

-- | Server listeners for delegation logic.

module Pos.Delegation.Listeners
       ( handlePsk
       , DlgListenerConstraint
       ) where

import           Universum

import           Formatting (build, sformat, shown, (%))
import           Mockable (CurrentTime, Delay, Mockable)
import           System.Wlog (WithLogger, logDebug, logWarning)

import           Pos.Binary.Delegation ()
import           Pos.Communication.Limits.Types (MessageLimited)
import           Pos.Communication.Protocol (Message)
import           Pos.Communication.Relay (DataMsg)
import           Pos.Core (ProxySKHeavy)
import           Pos.DB.Class (MonadBlockDBRead, MonadGState)
import           Pos.Delegation.Class (MonadDelegation)
import           Pos.Delegation.Configuration (HasDlgConfiguration)
import           Pos.Delegation.Logic (PskHeavyVerdict (..), processProxySKHeavy)
import           Pos.Lrc.Context (HasLrcContext)
import           Pos.StateLock (StateLock)
import           Pos.Util (HasLens')
import           Pos.Util.Verification (Unver, VerError (..), runVerify)

-- Message constraints we need to be defined.
type DlgMessageConstraint m
     = ( Message (DataMsg (Unver ProxySKHeavy))
       , MessageLimited (DataMsg (Unver ProxySKHeavy)) m
       )

-- | This is a subset of 'WorkMode'.
type DlgListenerConstraint ctx m
     = ( MonadIO m
       , MonadDelegation ctx m
       , MonadMask m
       , Mockable Delay m
       , Mockable CurrentTime m
       , MonadGState m
       , MonadBlockDBRead m
       , HasLens' ctx StateLock
       , HasLrcContext ctx
       , WithLogger m
       , DlgMessageConstraint m
       , HasDlgConfiguration)

handlePsk :: DlgListenerConstraint ctx m => Unver ProxySKHeavy -> m Bool
handlePsk pskUnver =
    case runVerify pskUnver of
        Right psk -> do
          logDebug $ sformat ("Got request to handle heavyweight psk: "%build) psk
          verdict <- processProxySKHeavy psk
          logDebug $ sformat ("The verdict for cert "%build%" is: "%shown) psk verdict
          case verdict of
              PHTipMismatch -> do
                  -- We're probably updating state over epoch, so
                  -- leaders can be calculated incorrectly. This is
                  -- really weird and must not happen. We'll just retry.
                  logWarning "Tip mismatch happened in delegation db!"
                  handlePsk pskUnver
              PHAdded -> pure True
              PHRemoved -> pure True
              _ -> pure False
        Left (VerError err) ->
            False <$ logWarning ("Somebody sent us invalid psk: " <> err)
