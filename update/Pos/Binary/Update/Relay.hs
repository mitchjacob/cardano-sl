module Pos.Binary.Update.Relay
       (
       ) where

import           Universum

import           Pos.Binary.Class (BiDec (..), BiEnc (..))
import           Pos.Binary.Core ()
import           Pos.Binary.Infra ()
import           Pos.Communication.Relay.Types (DataMsg (..))
import           Pos.Core (HasConfiguration)
import qualified Pos.Core.Update as U
import           Pos.Crypto (hash)
import           Pos.Util.Util (cborError)

----------------------------------------------------------------------------
-- Relay
----------------------------------------------------------------------------

instance HasConfiguration => BiEnc (DataMsg (U.UpdateProposal, [U.UpdateVote])) where
    encode = encode . dmContents
instance HasConfiguration => BiDec (DataMsg (U.UpdateProposal, [U.UpdateVote])) where
    decode = do
        c@(up, votes) <- decode
        let !id = hash up
        unless (all ((id ==) . U.uvProposalId) votes) $ cborError $
            "decode@DataMsg@Update: vote's uvProposalId must be equal UpId"
        pure $ DataMsg c

instance HasConfiguration => BiEnc (DataMsg U.UpdateVote) where
    encode = encode . dmContents
instance HasConfiguration => BiDec (DataMsg U.UpdateVote) where
    decode = DataMsg <$> decode
