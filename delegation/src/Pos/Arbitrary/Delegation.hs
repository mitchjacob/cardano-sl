-- | Arbitrary instances for Delegation types.

module Pos.Arbitrary.Delegation
       ( genDlgPayload
       ) where

import           Universum

import qualified Data.HashMap.Strict as HM
import qualified Data.Set as Set
import           Test.QuickCheck (Arbitrary (..), Gen, listOf)
import           Test.QuickCheck.Arbitrary.Generic (genericShrink)

import           Pos.Arbitrary.Core ()
import           Pos.Core (EpochIndex, HasConfiguration, HeavyDlgIndex (..))
import           Pos.Crypto (ProxySecretKey (..), createPsk)
import           Pos.Delegation.Types (DlgPayload (..))

genDlgPayload :: HasConfiguration => EpochIndex -> Gen DlgPayload
genDlgPayload epoch =
    DlgPayload . Set.fromList . toList . HM.fromList . map convert <$>
    listOf genPSK
  where
    convert psk = (pskIssuerPk psk, psk)
    genPSK = createPsk <$> arbitrary <*> arbitrary <*> pure (HeavyDlgIndex epoch)

instance HasConfiguration => Arbitrary DlgPayload where
    arbitrary = arbitrary >>= genDlgPayload
    shrink = genericShrink
