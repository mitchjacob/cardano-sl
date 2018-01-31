{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}

-- | This module is dedicated to support the verification granularity proposal.
-- See docs/proposals/serialization.md.

module Pos.Util.Verification
    ( Unver
    , getUnverUnsafe
    , mkUnver

    , VerM
    , verMFail
    , verMField

    , Verifiable(..)
    , runVerify
    , runVerifyFail
    ) where

import           Universum

import qualified Data.Text as T
import qualified Data.Text.Buildable
import           Formatting (bprint, build, (%))

import           Pos.Util.Util (eitherToThrow)

----------------------------------------------------------------------------
-- Unver wrapper
----------------------------------------------------------------------------

-- | Verification datatype. For now we support only two levels of data
-- verification.
newtype Unver a = Unver
    { getUnverUnsafe :: a
    } deriving (Eq, Ord, Functor)

instance Applicative Unver where
    pure = Unver
    (Unver a) <*> (Unver b) = Unver (a b)

instance Monad Unver where
    (Unver a) >>= foo = foo a

mkUnver :: a -> Unver a
mkUnver = Unver

----------------------------------------------------------------------------
-- VerM
----------------------------------------------------------------------------

newtype VerM c = VerM
    { getVerM :: Either [Text] c
    } deriving (Show, Eq, Functor, Applicative, Monad)

data VerError = VerError Text deriving (Show)

instance Exception VerError

verMFail :: Text -> VerM c
verMFail t = VerM (Left [t])

-- | Verifies some field, prefixing with the text value in case of
-- error. Prefix is supposed to be the record field name.
verMField :: Text -> VerM c -> VerM c
verMField p (VerM v) = VerM $ first (\x -> p:x) v

----------------------------------------------------------------------------
-- Verifiable
----------------------------------------------------------------------------

-- | Things that can be verified
class Verifiable a where
    verify :: a -> VerM a

runVerify :: (Verifiable a) => Unver a -> Either VerError a
runVerify (Unver a) = first (VerError . T.intercalate "." . reverse) . getVerM $ verify a

runVerifyFail :: (MonadThrow m, Verifiable a) => Unver a -> m a
runVerifyFail = eitherToThrow . runVerify

----------------------------------------------------------------------------
-- Instances
----------------------------------------------------------------------------

instance Buildable a => Buildable (Unver a) where
    build = bprint ("Unver "%build)
