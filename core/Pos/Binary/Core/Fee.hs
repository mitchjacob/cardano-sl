-- | Binary instances for transaction fee data.

module Pos.Binary.Core.Fee () where

import           Universum

import qualified Data.ByteString.Lazy as LBS
import           Data.Fixed (Nano)

import           Pos.Binary.Class (BiDec (..), BiEnc (..), decode, decodeKnownCborDataItem,
                                   decodeUnknownCborDataItem, encode, encodeKnownCborDataItem,
                                   encodeListLen, encodeUnknownCborDataItem, enforceSize)
import           Pos.Core.Common.Fee (Coeff (..), TxFeePolicy (..), TxSizeLinear (..))

instance BiEnc Coeff where
    encode (Coeff n) = encode n
instance BiDec Coeff where
    decode = Coeff <$> decode @Nano

instance BiEnc TxSizeLinear where
    encode (TxSizeLinear a b) = encodeListLen 2 <> encode a <> encode b
instance BiDec TxSizeLinear where
    decode = do
        enforceSize "TxSizeLinear" 2
        !a <- decode @Coeff
        !b <- decode @Coeff
        return $ TxSizeLinear a b

instance BiEnc TxFeePolicy where
    encode policy = case policy of
        TxFeePolicyTxSizeLinear txSizeLinear ->
            encodeListLen 2 <> encode (0 :: Word8)
                            <> encodeKnownCborDataItem txSizeLinear
        TxFeePolicyUnknown word8 bs          ->
            encodeListLen 2 <> encode word8
                            <> encodeUnknownCborDataItem (LBS.fromStrict bs)
instance BiDec TxFeePolicy where
    decode = do
        enforceSize "TxFeePolicy" 2
        tag <- decode @Word8
        case tag of
            0 -> TxFeePolicyTxSizeLinear <$> decodeKnownCborDataItem
            _ -> TxFeePolicyUnknown tag  <$> decodeUnknownCborDataItem
