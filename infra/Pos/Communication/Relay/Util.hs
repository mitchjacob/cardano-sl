module Pos.Communication.Relay.Util
       ( expectInv
       , expectData
       ) where

import           Universum

import           Pos.Communication.Relay.Types (DataMsg, InvMsg, InvOrData,
                                                RelayError (UnexpectedData, UnexpectedInv))

expectInv
    :: MonadThrow m
    => (InvMsg key -> m a) -> InvOrData key contents -> m a
expectInv call = either call (\_ -> throwM UnexpectedData)

expectData
    :: MonadThrow m
    => (DataMsg contents -> m a) -> InvOrData key contents -> m a
expectData call = either (\_ -> throwM UnexpectedInv) call
