-- | Binary serialization of network Txp types.

module Pos.Binary.Txp.Network
       (
       ) where

import           Universum

import           Pos.Binary.Class (BiDec (..), BiEnc (..))
import           Pos.Communication.Relay.Types (DataMsg (..))
import           Pos.Txp.Network.Types (TxMsgContents (..))

----------------------------------------------------------------------------
-- Network
----------------------------------------------------------------------------

instance BiEnc (DataMsg TxMsgContents) where
    encode (DataMsg (TxMsgContents txAux)) = encode txAux
instance BiDec (DataMsg TxMsgContents) where
    decode = DataMsg <$> (TxMsgContents <$> decode)
