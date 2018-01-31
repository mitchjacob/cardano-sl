module Pos.Binary.Infra.Relay
       (
       ) where

import           Universum

import           Pos.Binary.Class (BiDec (..), BiEnc (..))
import           Pos.Communication.Relay.Types (DataMsg (..), InvMsg (..), MempoolMsg (..),
                                                ReqMsg (..), ResMsg (..))
import           Pos.Util.Util (cborError)
import           Pos.Util.Verification (Unver, getUnverUnsafe)

instance BiEnc key => BiEnc (InvMsg key) where
    encode = encode . imKey
instance BiDec key => BiDec (InvMsg key) where
    decode = InvMsg <$> decode

instance BiEnc key => BiEnc (ReqMsg key) where
    encode = encode . rmKey
instance BiDec key => BiDec (ReqMsg key) where
    decode = ReqMsg <$> decode

instance BiEnc key => BiEnc (ResMsg key) where
    encode (ResMsg {..}) = encode (resKey, resOk)
instance BiDec key => BiDec (ResMsg key) where
    decode = uncurry ResMsg <$> decode

instance Typeable tag => BiEnc (MempoolMsg tag) where
    -- The extra byte is needed because time-warp doesn't work with
    -- possibly-empty messages. 228 was chosen as homage to @pva701
    encode MempoolMsg = encode (228 :: Word8)
instance Typeable tag => BiDec (MempoolMsg tag) where
    decode = do
        x <- decode @Word8
        when (x /= 228) $ cborError "wrong byte"
        pure MempoolMsg

instance (Typeable a, BiEnc (DataMsg a)) => BiEnc (DataMsg (Unver a)) where
    -- It's safe to use it here, because sometimes we want to
    -- propagate the data without verifying it.
    encode (DataMsg c) = encode $ DataMsg $ getUnverUnsafe c
