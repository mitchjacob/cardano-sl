-- | Binary serialization of core block types.

module Pos.Binary.Core.Blockchain
       (
       ) where

import           Universum

import           Pos.Binary.Class (BiDec (..), BiEnc (..), Decoder, Encoding, encodeListLen,
                                   enforceSize)
import           Pos.Binary.Core.Common ()
import           Pos.Core.Block (Block, BlockHeader, GenesisBlock, GenesisBlockHeader, MainBlock,
                                 MainBlockHeader)
import qualified Pos.Core.Block.Blockchain as T
import           Pos.Core.Configuration (HasConfiguration)
import           Pos.Crypto.Configuration (HasCryptoConfiguration, getProtocolMagic, protocolMagic)
import           Pos.Util.Util (cborError)
import           Pos.Util.Verification (Unver)


----------------------------------------------------------------------------
-- GenericHeader
----------------------------------------------------------------------------

encodeGenericHeader ::
       ( Typeable b
       , BiEnc (T.BHeaderHash b)
       , BiEnc (T.BodyProof b)
       , BiEnc (T.ConsensusData b)
       , BiEnc (T.ExtraHeaderData b)
       , HasCryptoConfiguration
       )
    => T.GenericBlockHeader b
    -> Encoding
encodeGenericHeader bh =
    encodeListLen 5 <>
    encode (getProtocolMagic protocolMagic) <>
    encode (T._gbhPrevBlock bh) <>
    encode (T._gbhBodyProof bh) <>
    encode (T._gbhConsensus bh) <>
    encode (T._gbhExtra bh)

decodeGenericHeader ::
       ( Typeable b
       , BiDec (T.BHeaderHash b)
       , BiDec (T.BodyProof b)
       , BiDec (Unver (T.ConsensusData b))
       , BiDec (T.ExtraHeaderData b)
       , HasCryptoConfiguration
       )
    => Decoder s (Unver (T.GenericBlockHeader b))
decodeGenericHeader = do
    enforceSize "GenericBlockHeader b" 5
    blockMagic <- decode
    when (blockMagic /= getProtocolMagic protocolMagic) $
        cborError $ "GenericBlockHeader failed with wrong magic: " <> show blockMagic
    gbhPrevBlock <- ({-# SCC "decode_header_prev" #-} decode)
    gbhBodyProof <- ({-# SCC "decode_header_body_proof" #-} decode)
    gbhConsensus <- ({-# SCC "decode_header_consensus" #-} decode)
    gbhExtra     <- ({-# SCC "decode_header_extra" #-} decode)
    pure $
        T.UnsafeGenericBlockHeader <$>
        pure gbhPrevBlock <*>
        pure gbhBodyProof <*>
        gbhConsensus <*>
        pure gbhExtra

instance HasCryptoConfiguration => BiEnc GenesisBlockHeader where
    encode = encodeGenericHeader
instance HasCryptoConfiguration => BiDec (Unver GenesisBlockHeader) where
    decode = decodeGenericHeader

instance HasConfiguration => BiEnc MainBlockHeader where
    encode = encodeGenericHeader
instance HasConfiguration => BiDec (Unver MainBlockHeader) where
    decode = decodeGenericHeader

-- BiEnc BlockHeader is derived automatically.
instance HasConfiguration => BiDec (Unver BlockHeader) where
    decode = do
        (v :: Either (Unver GenesisBlockHeader) (Unver MainBlockHeader)) <- decode
        pure $ either (fmap Left) (fmap Right) v

----------------------------------------------------------------------------
-- GenericBlock
----------------------------------------------------------------------------

encodeGenericBlock ::
       ( Typeable b
       , BiEnc (T.GenericBlockHeader b)
       , BiEnc (T.Body b)
       , BiEnc (T.ExtraBodyData b)
       , HasCryptoConfiguration
       )
    => T.GenericBlock b
    -> Encoding
encodeGenericBlock gb =
    encodeListLen 3 <>
    encode (T._gbHeader gb) <>
    encode (T._gbBody gb) <>
    encode (T._gbExtra gb)

decodeGenericBlock ::
       ( Typeable b
       , BiDec (Unver (T.GenericBlockHeader b))
       , BiDec (Unver (T.Body b))
       , BiDec (T.ExtraBodyData b)
       , HasCryptoConfiguration
       )
    => Decoder s (Unver (T.GenericBlock b))
decodeGenericBlock = do
    enforceSize "GenericBlock" 3
    gbHeader <- ({-# SCC "decode_block_header" #-} decode)
    gbBody   <- ({-# SCC "decode_block_body" #-} decode)
    gbExtra  <- ({-# SCC "decode_block_extra" #-} decode)
    pure $ T.UnsafeGenericBlock <$> gbHeader <*> gbBody <*> pure gbExtra

instance HasCryptoConfiguration => BiEnc GenesisBlock where
    encode = encodeGenericBlock
instance HasCryptoConfiguration => BiDec (Unver GenesisBlock) where
    decode = decodeGenericBlock

instance HasConfiguration => BiEnc MainBlock where
    encode = encodeGenericBlock
instance HasConfiguration => BiDec (Unver MainBlock) where
    decode = decodeGenericBlock

-- BiEnc Block is derived automatically.
instance HasConfiguration => BiDec (Unver Block) where
    decode = do
        (v :: Either (Unver GenesisBlock) (Unver MainBlock)) <- decode
        pure $ either (fmap Left) (fmap Right) v
