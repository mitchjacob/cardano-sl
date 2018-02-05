-- | Functionality related to 'Address' data type and related types.

module Pos.Core.Common.Address
       (
         -- * Re-exports
         Address (..)
       , StakeholderId

       -- * Formatting
       , addressF
       , addressDetailedF
       , decodeTextAddress

       -- * Spending data checks
       , checkAddrSpendingData
       , checkPubKeyAddress
       , checkScriptAddress
       , checkRedeemAddress

       -- * Utilities
       , addrSpendingDataToType
       , addrAttributesUnwrapped
       , deriveLvl2KeyPair
       , deriveFirstHDAddress

       -- * Pattern-matching helpers
       , isRedeemAddress
       , isUnknownAddressType
       , isBootstrapEraDistrAddress

       -- * Construction
       , IsBootstrapEraAddr (..)
       , makeAddress
       , makePubKeyAddress
       , makePubKeyAddressBoot
       , makeRootPubKeyAddress
       , makePubKeyHdwAddress
       , makeScriptAddress
       , makeRedeemAddress

       , createHDAddressNH
       , createHDAddressH

       -- * Maximal sizes (needed for tx creation)
       , largestPubKeyAddressBoot
       , maxPubKeyAddressSizeBoot
       , largestPubKeyAddressSingleKey
       , maxPubKeyAddressSizeSingleKey
       , largestHDAddressBoot
       , maxHDAddressSizeBoot

         -- * Internals
       , AddressHash
       , addressHash
       , unsafeAddressHash
       ) where

import           Universum

import           Crypto.Hash (Blake2b_224, Digest, SHA3_256)
import qualified Crypto.Hash as CryptoHash
import qualified Data.ByteString as BS
import           Data.ByteString.Base58 (Alphabet (..), bitcoinAlphabet, decodeBase58, encodeBase58)
import           Data.Hashable (Hashable (..))
import qualified Data.Text.Buildable as Buildable
import           Formatting (Format, bprint, build, builder, int, later, (%))
import           Serokell.Data.Memory.Units (Byte)
import           Serokell.Util (mapJson)

import           Pos.Binary.Class (BiDec, BiEnc, biSize)
import qualified Pos.Binary.Class as Bi
import           Pos.Binary.Crypto ()
import           Pos.Core.Common.Coin ()
import           Pos.Core.Common.Types (AddrAttributes (..), AddrSpendingData (..),
                                        AddrStakeDistribution (..), AddrType (..), Address (..),
                                        Address' (..), AddressHash, Script, StakeholderId)
import           Pos.Core.Constants (accountGenesisIndex, wAddressGenesisIndex)
import           Pos.Crypto.Hashing (AbstractHash (AbstractHash), hashHexF, shortHashF)
import           Pos.Crypto.HD (HDAddressPayload, HDPassphrase, ShouldCheckPassphrase (..),
                                deriveHDPassphrase, deriveHDPublicKey, deriveHDSecretKey,
                                packHDAddressAttr)
import           Pos.Crypto.Signing (EncryptedSecretKey, PassPhrase, PublicKey, RedeemPublicKey,
                                     SecretKey, deterministicKeyGen, emptyPassphrase, encToPublic,
                                     noPassEncrypt)
import           Pos.Data.Attributes (attrData, mkAttributes)

instance BiEnc Address => Hashable Address where
    hashWithSalt s = hashWithSalt s . Bi.serialize

----------------------------------------------------------------------------
-- Formatting, pretty-printing
----------------------------------------------------------------------------

instance Buildable AddrSpendingData where
    build =
        \case
            PubKeyASD pk -> bprint ("PubKeyASD " %build) pk
            ScriptASD script -> bprint ("ScriptASD "%build) script
            RedeemASD rpk -> bprint ("RedeemASD "%build) rpk
            UnknownASD tag _ -> bprint ("UnknownASD with tag "%int) tag

instance Buildable AddrStakeDistribution where
    build =
        \case
            BootstrapEraDistr -> "Bootstrap era distribution"
            SingleKeyDistr id ->
                bprint ("Single key distribution ("%shortHashF%")") id
            UnsafeMultiKeyDistr distr ->
                bprint ("Multi key distribution: "%mapJson) distr

instance Buildable AddrAttributes where
    build (AddrAttributes {..}) =
        bprint
            ("AddrAttributes { stake distribution: "%build%
             ", derivation path: "%builder%" }")
            aaStakeDistribution
            derivationPathBuilder
      where
        derivationPathBuilder =
            case aaPkDerivationPath of
                Nothing -> "{}"
                Just _  -> "{path is encrypted}"

-- | A formatter showing guts of an 'Address'.
addressDetailedF :: Format r (Address -> r)
addressDetailedF =
    later $ \Address {..} ->
        bprint (builder%" address with root "%hashHexF%", attributes: "%build)
            (formattedType addrType) addrRoot addrAttributes
  where
    formattedType =
        \case
            ATPubKey      -> "PubKey"
            ATScript      -> "Script"
            ATRedeem      -> "Redeem"
            ATUnknown tag -> "Unknown#" <> Buildable.build tag

-- | Currently we gonna use Bitcoin alphabet for representing addresses in
-- base58
addrAlphabet :: Alphabet
addrAlphabet = bitcoinAlphabet

addrToBase58 :: BiEnc Address => Address -> ByteString
addrToBase58 = encodeBase58 addrAlphabet . Bi.serialize'

instance BiEnc Address => Buildable Address where
    build = Buildable.build . decodeUtf8 @Text . addrToBase58

-- | Specialized formatter for 'Address'.
addressF :: BiEnc Address => Format r (Address -> r)
addressF = build

-- | A function which decodes base58-encoded 'Address'.
decodeTextAddress :: BiDec Address => Text -> Either Text Address
decodeTextAddress = decodeAddress . encodeUtf8
  where
    decodeAddress :: BiDec Address => ByteString -> Either Text Address
    decodeAddress bs = do
        let base58Err = "Invalid base58 representation of address"
        dbs <- maybeToRight base58Err $ decodeBase58 addrAlphabet bs
        Bi.decodeFull' dbs

----------------------------------------------------------------------------
-- Constructors
----------------------------------------------------------------------------

-- | Make an 'Address' from spending data and attributes.
makeAddress :: BiEnc Address' => AddrSpendingData -> AddrAttributes -> Address
makeAddress spendingData attributesUnwrapped =
    Address
    { addrRoot = addressHash address'
    , addrAttributes = attributes
    , ..
    }
  where
    addrType = addrSpendingDataToType spendingData
    attributes = mkAttributes attributesUnwrapped
    address' = Address' (addrType, spendingData, attributes)

-- | This newtype exists for clarity. It is used to tell pubkey
-- address creation functions whether an address is intended for
-- bootstrap era.
newtype IsBootstrapEraAddr = IsBootstrapEraAddr Bool

makePubKeyAddressImpl
    :: BiEnc Address'
    => Maybe HDAddressPayload
    -> IsBootstrapEraAddr
    -> PublicKey
    -> Address
makePubKeyAddressImpl path (IsBootstrapEraAddr isBootstrapEra) key =
    makeAddress spendingData attrs
  where
    spendingData = PubKeyASD key
    distr
        | isBootstrapEra = BootstrapEraDistr
        | otherwise = SingleKeyDistr (addressHash key)
    attrs =
        AddrAttributes {aaStakeDistribution = distr, aaPkDerivationPath = path}

-- | A function for making an address from 'PublicKey'.
makePubKeyAddress :: BiEnc Address' => IsBootstrapEraAddr -> PublicKey -> Address
makePubKeyAddress = makePubKeyAddressImpl Nothing

-- | A function for making an address from 'PublicKey' for bootstrap era.
makePubKeyAddressBoot :: BiEnc Address' => PublicKey -> Address
makePubKeyAddressBoot = makePubKeyAddress (IsBootstrapEraAddr True)

-- | This function creates a root public key address. Stake
-- distribution doesn't matter for root addresses because by design
-- nobody should even use these addresses as outputs, so we can put
-- arbitrary distribution there. We use bootstrap era distribution
-- because its representation is more compact.
makeRootPubKeyAddress :: BiEnc Address' => PublicKey -> Address
makeRootPubKeyAddress = makePubKeyAddressBoot

-- | A function for making an HDW address.
makePubKeyHdwAddress
    :: BiEnc Address'
    => IsBootstrapEraAddr
    -> HDAddressPayload    -- ^ Derivation path
    -> PublicKey
    -> Address
makePubKeyHdwAddress ibe path = makePubKeyAddressImpl (Just path) ibe

-- | A function for making an address from a validation 'Script'.  It
-- takes an optional 'StakeholderId'. If it's given, it will receive
-- the stake sent to the resulting 'Address'. Otherwise it's assumed
-- that an 'Address' is created for bootstrap era.
makeScriptAddress :: BiEnc Address' => Maybe StakeholderId -> Script -> Address
makeScriptAddress stakeholder scr = makeAddress spendingData attrs
  where
    spendingData = ScriptASD scr
    aaStakeDistribution = maybe BootstrapEraDistr SingleKeyDistr stakeholder
    attrs = AddrAttributes {aaPkDerivationPath = Nothing, ..}

-- | A function for making an address from 'RedeemPublicKey'.
makeRedeemAddress :: BiEnc Address' => RedeemPublicKey -> Address
makeRedeemAddress key = makeAddress spendingData attrs
  where
    spendingData = RedeemASD key
    attrs =
        AddrAttributes
        {aaStakeDistribution = BootstrapEraDistr, aaPkDerivationPath = Nothing}

-- | Create address from secret key in hardened way.
createHDAddressH
    :: BiEnc Address'
    => IsBootstrapEraAddr
    -> ShouldCheckPassphrase
    -> PassPhrase
    -> HDPassphrase
    -> EncryptedSecretKey
    -> [Word32]
    -> Word32
    -> Maybe (Address, EncryptedSecretKey)
createHDAddressH ibea scp passphrase hdPassphrase parent parentPath childIndex = do
    derivedSK <- deriveHDSecretKey scp passphrase parent childIndex
    let addressPayload = packHDAddressAttr hdPassphrase $ parentPath ++ [childIndex]
    let pk = encToPublic derivedSK
    return (makePubKeyHdwAddress ibea addressPayload pk, derivedSK)

-- | Create address from public key via non-hardened way.
createHDAddressNH
    :: BiEnc Address'
    => IsBootstrapEraAddr
    -> HDPassphrase
    -> PublicKey
    -> [Word32]
    -> Word32
    -> (Address, PublicKey)
createHDAddressNH ibea passphrase parent parentPath childIndex = do
    let derivedPK = deriveHDPublicKey parent childIndex
    let addressPayload = packHDAddressAttr passphrase $ parentPath ++ [childIndex]
    (makePubKeyHdwAddress ibea addressPayload derivedPK, derivedPK)

----------------------------------------------------------------------------
-- Checks
----------------------------------------------------------------------------

-- | Check whether given 'AddrSpendingData' corresponds to given
-- 'Address'.
checkAddrSpendingData :: (BiEnc Address') => AddrSpendingData -> Address -> Bool
checkAddrSpendingData asd Address {..} =
    addrRoot == addressHash address' && addrType == addrSpendingDataToType asd
  where
    address' = Address' (addrType, asd, addrAttributes)

-- | Check if given 'Address' is created from given 'PublicKey'
checkPubKeyAddress :: (BiEnc Address') => PublicKey -> Address -> Bool
checkPubKeyAddress pk = checkAddrSpendingData (PubKeyASD pk)

-- | Check if given 'Address' is created from given validation script
checkScriptAddress :: (BiEnc Address') => Script -> Address -> Bool
checkScriptAddress script = checkAddrSpendingData (ScriptASD script)

-- | Check if given 'Address' is created from given 'RedeemPublicKey'
checkRedeemAddress :: (BiEnc Address') => RedeemPublicKey -> Address -> Bool
checkRedeemAddress rpk = checkAddrSpendingData (RedeemASD rpk)

----------------------------------------------------------------------------
-- Hashing
----------------------------------------------------------------------------

unsafeAddressHash :: BiEnc a => a -> AddressHash b
unsafeAddressHash = AbstractHash . secondHash . firstHash
  where
    firstHash :: BiEnc a => a -> Digest SHA3_256
    firstHash = CryptoHash.hashlazy . Bi.serialize
    secondHash :: Digest SHA3_256 -> Digest Blake2b_224
    secondHash = CryptoHash.hash

addressHash :: BiEnc a => a -> AddressHash a
addressHash = unsafeAddressHash

----------------------------------------------------------------------------
-- Utils
----------------------------------------------------------------------------

-- | Convert 'AddrSpendingData' to the corresponding 'AddrType'.
addrSpendingDataToType :: AddrSpendingData -> AddrType
addrSpendingDataToType =
    \case
        PubKeyASD {} -> ATPubKey
        ScriptASD {} -> ATScript
        RedeemASD {} -> ATRedeem
        UnknownASD tag _ -> ATUnknown tag

-- | Get 'AddrAttributes' from 'Address'.
addrAttributesUnwrapped :: Address -> AddrAttributes
addrAttributesUnwrapped = attrData . addrAttributes

-- | Makes account secret key for given wallet set.
deriveLvl2KeyPair
    :: BiEnc Address'
    => IsBootstrapEraAddr
    -> ShouldCheckPassphrase
    -> PassPhrase
    -> EncryptedSecretKey -- ^ key of wallet
    -> Word32 -- ^ account derivation index
    -> Word32 -- ^ address derivation index
    -> Maybe (Address, EncryptedSecretKey)
deriveLvl2KeyPair ibea scp passphrase wsKey accountIndex addressIndex = do
    wKey <- deriveHDSecretKey scp passphrase wsKey accountIndex
    let hdPass = deriveHDPassphrase $ encToPublic wsKey
    -- We don't need to check passphrase twice
    createHDAddressH ibea (ShouldCheckPassphrase False) passphrase hdPass wKey [accountIndex] addressIndex

deriveFirstHDAddress
    :: BiEnc Address'
    => IsBootstrapEraAddr
    -> PassPhrase
    -> EncryptedSecretKey -- ^ key of wallet set
    -> Maybe (Address, EncryptedSecretKey)
deriveFirstHDAddress ibea passphrase wsKey =
    deriveLvl2KeyPair ibea (ShouldCheckPassphrase False) passphrase wsKey accountGenesisIndex wAddressGenesisIndex

----------------------------------------------------------------------------
-- Pattern-matching helpers
----------------------------------------------------------------------------

-- | Check whether an 'Address' is redeem address.
isRedeemAddress :: Address -> Bool
isRedeemAddress Address {..} =
    case addrType of
        ATRedeem -> True
        _        -> False

isUnknownAddressType :: Address -> Bool
isUnknownAddressType Address {..} =
    case addrType of
        ATUnknown {} -> True
        _            -> False

-- | Check whether an 'Address' has bootstrap era stake distribution.
isBootstrapEraDistrAddress :: Address -> Bool
isBootstrapEraDistrAddress (addrAttributesUnwrapped -> AddrAttributes {..}) =
    case aaStakeDistribution of
        BootstrapEraDistr -> True
        _                 -> False

----------------------------------------------------------------------------
-- Maximal size
----------------------------------------------------------------------------

-- | Largest (considering size of serialized data) PubKey address with
-- BootstrapEra distribution. Actual size depends on CRC32 value which
-- is serialized using var-length encoding.
largestPubKeyAddressBoot :: BiEnc Address' => Address
largestPubKeyAddressBoot = makePubKeyAddressBoot goodPk

-- | Maximal size of PubKey address with BootstrapEra
-- distribution (43).
maxPubKeyAddressSizeBoot :: (BiEnc Address, BiEnc Address') => Byte
maxPubKeyAddressSizeBoot = biSize largestPubKeyAddressBoot

-- | Largest (considering size of serialized data) PubKey address with
-- SingleKey distribution. Actual size depends on CRC32 value which
-- is serialized using var-length encoding.
largestPubKeyAddressSingleKey :: BiEnc Address' => Address
largestPubKeyAddressSingleKey =
    makePubKeyAddress (IsBootstrapEraAddr False) goodPk

-- | Maximal size of PubKey address with SingleKey
-- distribution (78).
maxPubKeyAddressSizeSingleKey :: (BiEnc Address, BiEnc Address') => Byte
maxPubKeyAddressSizeSingleKey = biSize largestPubKeyAddressSingleKey

-- | Largest (considering size of serialized data) HD address with
-- BootstrapEra distribution. Actual size depends on CRC32 value which
-- is serialized using var-length encoding.
largestHDAddressBoot :: BiEnc Address' => Address
largestHDAddressBoot =
    case deriveLvl2KeyPair
             (IsBootstrapEraAddr True)
             (ShouldCheckPassphrase False)
             emptyPassphrase
             encSK
             maxBound
             maxBound of
        Nothing        -> error "largestHDAddressBoot failed"
        Just (addr, _) -> addr
  where
    encSK = noPassEncrypt goodSk

-- | Maximal size of HD address with BootstrapEra
-- distribution (76).
maxHDAddressSizeBoot :: (BiEnc Address, BiEnc Address') => Byte
maxHDAddressSizeBoot = biSize largestHDAddressBoot

-- Public key and secret key for which we know that they produce
-- largest addresses in all cases we are interested in. It was checked
-- manually.
goodSkAndPk :: (PublicKey, SecretKey)
goodSkAndPk = deterministicKeyGen $ BS.replicate 32 0

goodPk :: PublicKey
goodPk = fst goodSkAndPk

goodSk :: SecretKey
goodSk = snd goodSkAndPk
