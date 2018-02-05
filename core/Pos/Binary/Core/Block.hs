-- | Serialization of core types from 'Pos.Core.Block'.

module Pos.Binary.Core.Block
       (
       ) where

import           Universum

import           Pos.Binary.Class (BiDec (..), BiEnc (..), Cons (..), Field (..), deriveSimpleBi,
                                   encodeListLen, enforceSize)
import           Pos.Binary.Core.Delegation ()
import           Pos.Binary.Core.Txp ()
import qualified Pos.Core.Block.Blockchain as Core
import qualified Pos.Core.Block.Genesis.Chain as BC
import qualified Pos.Core.Block.Genesis.Types as BC
import qualified Pos.Core.Block.Main.Chain as BC
import qualified Pos.Core.Block.Main.Types as BC
import           Pos.Core.Configuration (HasConfiguration)
import           Pos.Core.Update.Types (BlockVersion, SoftwareVersion)
import           Pos.Crypto (Hash)
import           Pos.Util.Util (cborError)
import           Pos.Util.Verification (Unver, mkUnver)

----------------------------------------------------------------------------
-- MainBlock
----------------------------------------------------------------------------

instance BiEnc (Core.BodyProof BC.MainBlockchain) where
    encode bc =  encodeListLen 4
              <> encode (BC.mpTxProof bc)
              <> encode (BC.mpMpcProof bc)
              <> encode (BC.mpDlgProof bc)
              <> encode (BC.mpUpdateProof bc)
instance BiDec (Core.BodyProof BC.MainBlockchain) where
    decode = do
        enforceSize "Core.BodyProof BC.MainBlockChain" 4
        BC.MainProof <$> decode <*>
                         decode <*>
                         decode <*>
                         decode

instance HasConfiguration => BiEnc BC.BlockSignature where
    encode input = case input of
        BC.BlockSignature sig       -> encodeListLen 2 <> encode (0 :: Word8) <> encode sig
        BC.BlockPSignatureLight pxy -> encodeListLen 2 <> encode (1 :: Word8) <> encode pxy
        BC.BlockPSignatureHeavy pxy -> encodeListLen 2 <> encode (2 :: Word8) <> encode pxy
instance HasConfiguration => BiDec (Unver BC.BlockSignature) where
    decode = do
        enforceSize "BlockSignature" 2
        tag <- decode @Word8
        case tag of
            0 -> mkUnver . BC.BlockSignature <$> decode
            1 -> fmap BC.BlockPSignatureLight <$> decode
            2 -> fmap BC.BlockPSignatureHeavy <$> decode
            _ -> cborError $ "decode@BlockSignature: unknown tag: " <> show tag

instance HasConfiguration => BiEnc (BC.ConsensusData BC.MainBlockchain) where
    encode cd =  encodeListLen 4
              <> encode (BC._mcdSlot cd)
              <> encode (BC._mcdLeaderKey cd)
              <> encode (BC._mcdDifficulty cd)
              <> encode (BC._mcdSignature cd)
instance HasConfiguration =>
         BiDec (Unver (BC.ConsensusData BC.MainBlockchain)) where
    decode = do
        enforceSize "BC.ConsensusData BC.MainBlockchain)" 4
        slot <- decode
        leaderKey <- decode
        difficulty <- decode
        (signature :: Unver BC.BlockSignature) <- decode
        pure $ BC.MainConsensusData slot leaderKey difficulty <$> signature

instance HasConfiguration => BiEnc (BC.Body BC.MainBlockchain) where
    encode bc =  encodeListLen 4
              <> encode (BC._mbTxPayload  bc)
              <> encode (BC._mbSscPayload bc)
              <> encode (BC._mbDlgPayload bc)
              <> encode (BC._mbUpdatePayload bc)
instance HasConfiguration => BiDec (Unver (BC.Body BC.MainBlockchain)) where
    decode = do
        enforceSize "BC.Body BC.MainBlockchain" 4
        txPayload <- decode
        sscPayload <- decode
        dlgPayload <- decode
        updatePayload <- decode
        pure $
            BC.MainBody txPayload sscPayload <$> dlgPayload <*>
                pure updatePayload

deriveSimpleBi ''BC.MainExtraHeaderData [
    Cons 'BC.MainExtraHeaderData [
        Field [| BC._mehBlockVersion    :: BlockVersion              |],
        Field [| BC._mehSoftwareVersion :: SoftwareVersion           |],
        Field [| BC._mehAttributes      :: BC.BlockHeaderAttributes  |],
        Field [| BC._mehEBDataProof     :: Hash BC.MainExtraBodyData |]
    ]]

deriveSimpleBi ''BC.MainExtraBodyData [
    Cons 'BC.MainExtraBodyData [
        Field [| BC._mebAttributes :: BC.BlockBodyAttributes |]
    ]]

instance HasConfiguration => BiEnc BC.MainToSign where
    encode mts = encodeListLen 5
               <> encode (BC._msHeaderHash mts)
               <> encode (BC._msBodyProof mts)
               <> encode (BC._msSlot mts)
               <> encode (BC._msChainDiff mts)
               <> encode (BC._msExtraHeader mts)
instance HasConfiguration => BiDec BC.MainToSign where
    decode = do
        enforceSize "BC.MainToSign" 5
        BC.MainToSign <$> decode <*>
                          decode <*>
                          decode <*>
                          decode <*>
                          decode

-- ----------------------------------------------------------------------------
-- -- GenesisBlock
-- ----------------------------------------------------------------------------

deriveSimpleBi ''BC.GenesisExtraHeaderData [
    Cons 'BC.GenesisExtraHeaderData [
        Field [| BC._gehAttributes :: BC.GenesisHeaderAttributes |]
    ]]

deriveSimpleBi ''BC.GenesisExtraBodyData [
    Cons 'BC.GenesisExtraBodyData [
        Field [| BC._gebAttributes :: BC.GenesisBodyAttributes |]
    ]]

instance BiEnc (BC.BodyProof BC.GenesisBlockchain) where
    encode (BC.GenesisProof h) = encode h
instance BiDec (BC.BodyProof BC.GenesisBlockchain) where
    decode = BC.GenesisProof <$> decode

instance BiEnc (BC.ConsensusData BC.GenesisBlockchain) where
    encode bc =  encodeListLen 2
              <> encode (BC._gcdEpoch bc)
              <> encode (BC._gcdDifficulty bc)
instance BiDec (Unver (BC.ConsensusData BC.GenesisBlockchain)) where
    decode = do
        enforceSize "BC.ConsensusData BC.GenesisBlockchain" 2
        d <- BC.GenesisConsensusData <$> decode <*> decode
        pure $ pure d

instance BiEnc (BC.Body BC.GenesisBlockchain) where
    encode = encode . BC._gbLeaders
instance BiDec (Unver (BC.Body BC.GenesisBlockchain)) where
    decode = pure . BC.GenesisBody <$> decode
