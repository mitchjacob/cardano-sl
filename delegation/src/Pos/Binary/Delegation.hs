-- | Delegation types serialization.

module Pos.Binary.Delegation
       (
       ) where

import           Universum

import           Pos.Binary.Class (BiDec (..), BiEnc (..), encodeListLen, enforceSize)
import           Pos.Binary.Core ()
import           Pos.Binary.Crypto ()
import           Pos.Communication.Relay.Types (DataMsg (..))
import           Pos.Core (ProxySKHeavy)
import           Pos.Core.Configuration (HasConfiguration)
import           Pos.Delegation.Types (DlgUndo (..))
import           Pos.Util.Verification (Unver, getUnverUnsafe)

instance HasConfiguration => BiEnc DlgUndo where
    encode DlgUndo{..} =
        encodeListLen 2 <>
        encode duPsks <>
        encode duPrevEpochPosted
instance HasConfiguration => BiDec DlgUndo where
    decode = do
        enforceSize "DlgUndo" 2
        -- It's ok to use getUnverUnsafe here, because DlgUndo is
        -- never meant to be received from unsafe places like
        -- network. We only put/get it from DB, which is safe to
        -- assume to be consistent.
        duPsks <- map getUnverUnsafe <$> decode
        duPrevEpochPosted <- decode
        pure DlgUndo{..}

instance HasConfiguration => BiEnc (DataMsg ProxySKHeavy) where
    encode = encode . dmContents

instance HasConfiguration => BiDec (DataMsg (Unver ProxySKHeavy)) where
    decode = DataMsg <$> decode
