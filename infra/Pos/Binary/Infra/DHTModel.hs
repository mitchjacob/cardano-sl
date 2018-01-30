-- | Provides functionality of representing `Bi` instances as correct
-- `Message`s used by time-warp.

module Pos.Binary.Infra.DHTModel () where

import           Universum

import           Network.Kademlia as K
import           Network.Kademlia.HashNodeId (HashId (..))
import           Network.Kademlia.Instance (BanState)
import           Network.Kademlia.Tree as K
import           Network.Kademlia.Types as K

import           Pos.Binary.Class (BiDec (..), BiEnc (..), encodeListLen, enforceSize,
                                   genericDecode, genericEncode)
import           Pos.DHT.Model.Types (DHTData (..), DHTKey (..))

instance BiEnc DHTKey where
    encode (DHTKey (HashId bs)) = encode bs
instance BiDec DHTKey where
    decode = DHTKey . HashId <$> decode

instance BiEnc DHTData where
    encode (DHTData unit) = encode unit
instance BiDec DHTData where
    decode = DHTData <$> decode

-- CSL-1296: Orphan (inefficient) Kademlia instances.

instance BiEnc PingInfo where
    encode = genericEncode
instance BiDec PingInfo where
    decode = genericDecode

instance BiEnc i => BiEnc (K.Node i) where
    encode = genericEncode
instance BiDec i => BiDec (K.Node i) where
    decode = genericDecode

instance BiEnc BanState where
    encode = genericEncode
instance BiDec BanState where
    decode = genericDecode

instance BiEnc K.Peer where
    encode p =
        encodeListLen 2 <> encode (K.peerHost p) <> encode (K.unwrapPort . K.peerPort $ p)
instance BiDec K.Peer where
    decode   = do
        enforceSize "Kademlia.Peer" 2
        K.Peer <$> decode <*> (K.wrapPort <$> decode)

instance BiEnc i => BiEnc (K.NodeTreeElem i) where
    encode = genericEncode
instance BiDec i => BiDec (K.NodeTreeElem i) where
    decode = genericDecode

instance BiEnc i => BiEnc (K.NodeTree i) where
    encode = genericEncode
instance BiDec i => BiDec (K.NodeTree i) where
    decode = genericDecode

instance BiEnc i => BiEnc (K.KademliaSnapshot i) where
    encode = genericEncode
instance BiDec i => BiDec (K.KademliaSnapshot i) where
    decode = genericDecode
