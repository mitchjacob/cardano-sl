module Pos.Binary.Ssc.Relay
       (
       ) where

import           Universum

import           Pos.Binary.Class (BiDec (..), BiEnc (..), encodeListLen, enforceSize)
import           Pos.Communication.Relay.Types (DataMsg (..))
import           Pos.Core.Configuration (HasConfiguration)
import qualified Pos.Ssc.Message as T

instance BiEnc (DataMsg T.MCCommitment) where
    encode (DataMsg (T.MCCommitment signedComm)) = encode signedComm
instance BiDec (DataMsg T.MCCommitment) where
    decode = DataMsg . T.MCCommitment <$> decode

instance BiEnc (DataMsg T.MCOpening) where
    encode (DataMsg (T.MCOpening sId opening)) = encodeListLen 2 <> encode sId <> encode opening
instance BiDec (DataMsg T.MCOpening) where
    decode = do
        enforceSize "DataMsg T.MCOpening" 2
        DataMsg <$> (T.MCOpening <$> decode <*> decode)

instance BiEnc (DataMsg T.MCShares) where
    encode (DataMsg (T.MCShares sId innerMap)) = encodeListLen 2 <> encode sId <> encode innerMap
instance BiDec (DataMsg T.MCShares) where
    decode = do
        enforceSize "DataMsg T.MCShares" 2
        DataMsg <$> (T.MCShares <$> decode <*> decode)

instance HasConfiguration => BiEnc (DataMsg T.MCVssCertificate) where
    encode (DataMsg (T.MCVssCertificate vss)) = encode vss
instance HasConfiguration => BiDec (DataMsg T.MCVssCertificate) where
    decode = DataMsg . T.MCVssCertificate <$> decode
