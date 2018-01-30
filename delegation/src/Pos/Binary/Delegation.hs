-- | Delegation types serialization.

module Pos.Binary.Delegation
       (
       ) where

import           Universum

import           Pos.Binary.Class (BiDec (..), BiEnc (..), Cons (..), Field (..), deriveSimpleBiCxt)
import           Pos.Binary.Core ()
import           Pos.Binary.Crypto ()
import           Pos.Communication.Types.Relay (DataMsg (..))
import           Pos.Core (ProxySKHeavy, StakeholderId)
import           Pos.Core.Configuration (HasConfiguration)
import           Pos.Delegation.Types (DlgUndo (..))
import           Pos.Util.Verification (Unver)

deriveSimpleBiCxt [t|HasConfiguration|] ''DlgUndo [
    Cons 'DlgUndo [
        Field [| duPsks            :: [ProxySKHeavy]        |],
        Field [| duPrevEpochPosted :: HashSet StakeholderId |]
    ]]

instance HasConfiguration => BiEnc (DataMsg ProxySKHeavy) where
    encode = encode . dmContents
instance HasConfiguration => BiDec (DataMsg (Unver ProxySKHeavy)) where
    decode = DataMsg <$> decode
