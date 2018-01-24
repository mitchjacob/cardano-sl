{-# LANGUAGE TypeOperators #-}

-- | Miscellaneous instances, etc. Related to the genesis blockchain of course.

module Pos.Core.Block.Genesis.Instances
       (
       ) where

import           Universum

import qualified Data.Text.Buildable as Buildable
import           Formatting (bprint, build, int, sformat, stext, (%))
import           Serokell.Util (Color (Magenta), colorize, listJson)

import           Pos.Binary.Class (BiEnc)
import           Pos.Binary.Core.Block ()
import           Pos.Core.Block.Blockchain (GenericBlock (..), GenericBlockHeader (..), gbHeader,
                                            gbhConsensus)
import           Pos.Core.Block.Genesis.Chain (Body (..), ConsensusData (..))
import           Pos.Core.Block.Genesis.Lens (gcdDifficulty, gcdEpoch)
import           Pos.Core.Block.Genesis.Types (GenesisBlock, GenesisBlockHeader, GenesisBlockchain)
import           Pos.Core.Block.Union.Types (BlockHeader, blockHeaderHash)
import           Pos.Core.Class (HasDifficulty (..), HasEpochIndex (..), HasEpochOrSlot (..),
                                 HasHeaderHash (..), IsGenesisHeader, IsHeader)
import           Pos.Core.Common (HeaderHash)
import           Pos.Core.Slotting.Types (EpochOrSlot (..))
import           Pos.Crypto (hashHexF)

----------------------------------------------------------------------------
-- Buildable
----------------------------------------------------------------------------

instance BiEnc BlockHeader => Buildable GenesisBlockHeader where
    build gbh@UnsafeGenericBlockHeader {..} =
        bprint
            ("GenesisBlockHeader:\n"%
             "    hash: "%hashHexF%"\n"%
             "    previous block: "%hashHexF%"\n"%
             "    epoch: "%build%"\n"%
             "    difficulty: "%int%"\n"
            )
            gbhHeaderHash
            _gbhPrevBlock
            _gcdEpoch
            _gcdDifficulty
      where
        gbhHeaderHash :: HeaderHash
        gbhHeaderHash = blockHeaderHash $ Left gbh
        GenesisConsensusData {..} = _gbhConsensus

instance BiEnc BlockHeader => Buildable GenesisBlock where
    build UnsafeGenericBlock {..} =
        bprint
            (stext%":\n"%
             "  "%build%
             stext
            )
            (colorize Magenta "GenesisBlock")
            _gbHeader
            formatLeaders
      where
        GenesisBody {..} = _gbBody
        formatIfNotNull formatter l = if null l then mempty else sformat formatter l
        formatLeaders = formatIfNotNull
            ("  leaders: "%listJson%"\n") _gbLeaders

----------------------------------------------------------------------------
-- Pos.Core.Class
----------------------------------------------------------------------------

instance HasEpochIndex GenesisBlock where
    epochIndexL = gbHeader . gbhConsensus . gcdEpoch

instance HasEpochIndex GenesisBlockHeader where
    epochIndexL = gbhConsensus . gcdEpoch

instance HasEpochOrSlot GenesisBlockHeader where
    getEpochOrSlot = EpochOrSlot . Left . _gcdEpoch . _gbhConsensus

instance HasEpochOrSlot GenesisBlock where
    getEpochOrSlot = getEpochOrSlot . _gbHeader

-- NB. it's not a mistake that these instances require @Bi BlockHeader@
-- instead of @Bi GenesisBlockHeader@. We compute header's hash by
-- converting it to a BlockHeader first.

instance BiEnc BlockHeader =>
         HasHeaderHash GenesisBlockHeader where
    headerHash = blockHeaderHash . Left

instance BiEnc BlockHeader =>
         HasHeaderHash GenesisBlock where
    headerHash = blockHeaderHash . Left . _gbHeader

instance HasDifficulty (ConsensusData GenesisBlockchain) where
    difficultyL = gcdDifficulty

instance HasDifficulty GenesisBlockHeader where
    difficultyL = gbhConsensus . difficultyL

instance HasDifficulty GenesisBlock where
    difficultyL = gbHeader . difficultyL

instance BiEnc BlockHeader => IsHeader GenesisBlockHeader
instance BiEnc BlockHeader => IsGenesisHeader GenesisBlockHeader
