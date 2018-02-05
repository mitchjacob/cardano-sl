{-# LANGUAGE RankNTypes #-}

-- | Common functionality related to Misc DB.

module Pos.DB.Misc.Common
       ( miscGetBi
       , miscPutBi
       ) where

import           Universum

import           Pos.Binary.Class (BiDec, BiEnc)
import           Pos.DB.Class (DBTag (..), MonadDB, MonadDBRead)
import           Pos.DB.Functions (dbGetBi, dbPutBi)

miscGetBi
    :: forall v m . (MonadDBRead m, BiDec v)
    => ByteString -> m (Maybe v)
miscGetBi = dbGetBi MiscDB

miscPutBi
    :: forall v m . (MonadDB m, BiEnc v)
    => ByteString -> v -> m ()
miscPutBi = dbPutBi MiscDB
