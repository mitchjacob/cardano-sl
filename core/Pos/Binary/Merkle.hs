-- | Merkle tree-related serialization

module Pos.Binary.Merkle () where

import           Universum

import           Pos.Binary.Class (BiDec (..), BiEnc (..), Raw)
import           Pos.Crypto.Hashing (Hash)
import           Pos.Merkle (MerkleRoot (..), MerkleTree (..), mkMerkleTree)

-- This instance is both faster and more space-efficient (as confirmed by a
-- benchmark). Hashing turns out to be faster than decoding extra data.
instance (BiEnc a, BiEnc (Hash Raw)) => BiEnc (MerkleTree a) where
    encode = encode . toList
instance (BiEnc a, BiDec a, BiDec (Hash Raw)) => BiDec (MerkleTree a) where
    decode = mkMerkleTree <$> decode

instance (BiEnc a, BiEnc (Hash Raw)) => BiEnc (MerkleRoot a) where
    encode = encode . getMerkleRoot
instance (BiDec a, BiDec (Hash Raw)) => BiDec (MerkleRoot a) where
    decode = MerkleRoot <$> decode
