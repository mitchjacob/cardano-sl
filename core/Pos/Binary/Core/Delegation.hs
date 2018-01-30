{-# LANGUAGE DataKinds #-}

-- | Delegation types serialization.

module Pos.Binary.Core.Delegation () where

import           Nub (ordNub)
import           Universum

import qualified Data.Set as S

import           Pos.Binary.Class (BiDec (..), BiEnc (..))
import           Pos.Binary.Core.Slotting ()
import           Pos.Binary.Crypto ()
import           Pos.Core.Delegation (DlgPayload (..), HeavyDlgIndex (..), LightDlgIndices (..),
                                      ProxySKHeavy)
import           Pos.Util.Verification (Unver)

instance BiEnc HeavyDlgIndex where
    encode = encode . getHeavyDlgIndex
instance BiDec HeavyDlgIndex where
    decode = HeavyDlgIndex <$> decode

instance BiEnc LightDlgIndices where
    encode = encode . getLightDlgIndices
instance BiDec LightDlgIndices where
    decode = LightDlgIndices <$> decode

instance BiEnc DlgPayload where
    encode = encode . S.toList . getDlgPayload
instance BiDec (Unver DlgPayload) where
    decode = do
        (psks :: [Unver ProxySKHeavy]) <- decode
        let asSet :: Unver (Set ProxySKHeavy)
            asSet = S.fromList <$> (sequence psks :: Unver [ProxySKHeavy])
        when (length psks /= length (ordNub psks)) $
              fail "DlgPayload is not a set: it has duplicates"
        pure $ DlgPayload <$> asSet
