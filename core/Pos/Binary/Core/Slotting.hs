-- | Binary serialization of slotting types.

module Pos.Binary.Core.Slotting
       (
       ) where

import           Universum

import           Control.Lens (_Left)

import           Pos.Binary.Class (BiDec (..), BiEnc (..), Cons (..), Field (..), deriveSimpleBiCxt)
import           Pos.Core.Configuration.Protocol (HasProtocolConstants)
import qualified Pos.Core.Slotting as T
import           Pos.Util.Util (toCborError)

instance BiEnc T.Timestamp where
    encode (T.Timestamp ms) = encode . toInteger $ ms
instance BiDec T.Timestamp where
    decode = T.Timestamp . fromIntegral <$> decode @Integer

instance BiEnc T.TimeDiff where
    encode = encode . toInteger
instance BiDec T.TimeDiff where
    decode = fromInteger <$> decode

instance BiEnc T.EpochIndex where
    encode (T.EpochIndex epoch) = encode epoch
instance BiDec T.EpochIndex where
    decode = T.EpochIndex <$> decode

instance HasProtocolConstants => BiEnc T.LocalSlotIndex where
    encode = encode . T.getSlotIndex
instance HasProtocolConstants => BiDec T.LocalSlotIndex where
    decode = do
        word16 <- decode @Word16
        toCborError $
            over _Left ("decode@LocalSlotIndex: " <>) $
            T.mkLocalSlotIndex word16

deriveSimpleBiCxt [t| HasProtocolConstants |] ''T.SlotId [
    Cons 'T.SlotId [
        Field [| T.siEpoch :: T.EpochIndex     |],
        Field [| T.siSlot  :: T.LocalSlotIndex |]
    ]]

instance HasProtocolConstants => BiEnc T.EpochOrSlot where
    encode (T.EpochOrSlot e) = encode e
instance HasProtocolConstants => BiDec T.EpochOrSlot where
    decode = T.EpochOrSlot <$> decode @(Either T.EpochIndex T.SlotId)

instance BiEnc T.SlotCount where
    encode = encode . T.getSlotCount
instance BiDec T.SlotCount where
    decode = T.SlotCount <$> decode
