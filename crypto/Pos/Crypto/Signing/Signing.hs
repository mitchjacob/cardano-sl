-- | Signing done with public/private keys.

module Pos.Crypto.Signing.Signing
       (
       -- * Keys
         emptyPass
       , keyGen
       , deterministicKeyGen

       -- * Signing and verification
       , sign
       , checkSig                      -- reexport
       , fullSignatureHexF
       , parseFullSignature
       , mkSigned

       -- * Versions for raw bytestrings
       , signRaw
       , checkSigRaw                   -- reexport

       -- * Proxy signature scheme
       , fullProxyCertHexF
       , parseFullProxyCert
       , proxySign
       , proxyVerify

       , module Pos.Crypto.Signing.Types.Signing
       ) where

import           Universum

import qualified Cardano.Crypto.Wallet as CC
import           Crypto.Random (MonadRandom, getRandomBytes)
import           Data.ByteArray (ScrubbedBytes)
import qualified Data.ByteString as BS
import           Data.Coerce (coerce)
import           Formatting (Format, build, later, sformat, (%))
import qualified Serokell.Util.Base16 as B16

import           Pos.Binary.Class (BiEnc, Raw)
import qualified Pos.Binary.Class as Bi
import           Pos.Binary.Crypto ()
import           Pos.Crypto.Configuration (HasCryptoConfiguration)
import           Pos.Crypto.Signing.Check (checkSig, checkSigRaw)
import           Pos.Crypto.Signing.Tag (signTag)
import           Pos.Crypto.Signing.Types.Signing
import           Pos.Crypto.Signing.Types.Tag (SignTag)

----------------------------------------------------------------------------
-- Keys, key generation & printing & decoding
----------------------------------------------------------------------------

emptyPass :: ScrubbedBytes
emptyPass = mempty

-- TODO: this is just a placeholder for actual (not ready yet) derivation
-- of keypair from seed in cardano-crypto API
createKeypairFromSeed :: BS.ByteString -> (CC.XPub, CC.XPrv)
createKeypairFromSeed seed =
    let prv = CC.generate seed emptyPass
    in  (CC.toXPub prv, prv)

-- | Generate a key pair. It's recommended to run it with 'runSecureRandom'
-- from "Pos.Crypto.Random" because the OpenSSL generator is probably safer
-- than the default IO generator.
keyGen :: MonadRandom m => m (PublicKey, SecretKey)
keyGen = do
    seed <- getRandomBytes 32
    let (pk, sk) = createKeypairFromSeed seed
    return (PublicKey pk, SecretKey sk)

-- | Create key pair deterministically from 32 bytes.
deterministicKeyGen :: BS.ByteString -> (PublicKey, SecretKey)
deterministicKeyGen seed =
    bimap PublicKey SecretKey (createKeypairFromSeed seed)

----------------------------------------------------------------------------
-- Signatures
----------------------------------------------------------------------------

-- | Formatter for 'Signature' to show it in hex.
fullSignatureHexF :: Format r (Signature a -> r)
fullSignatureHexF = later $ \(Signature x) ->
    B16.formatBase16 . CC.unXSignature $ x

-- | Parse 'Signature' from base16 encoded string.
parseFullSignature :: Text -> Either Text (Signature a)
parseFullSignature s = do
    b <- B16.decode s
    Signature <$> first fromString (CC.xsignature b)

-- | Encode something with 'Binary' and sign it.
sign
    :: (HasCryptoConfiguration, BiEnc a)
    => SignTag         -- ^ See docs for 'SignTag'
    -> SecretKey
    -> a
    -> Signature a
sign t k = coerce . signRaw (Just t) k . Bi.serialize'

-- | Sign a bytestring.
signRaw
    :: HasCryptoConfiguration
    => Maybe SignTag   -- ^ See docs for 'SignTag'. Unlike in 'sign', we
                       -- allow no tag to be provided just in case you need
                       -- to sign /exactly/ the bytestring you provided
    -> SecretKey
    -> ByteString
    -> Signature Raw
signRaw mbTag (SecretKey k) x = Signature (CC.sign emptyPass k (tag <> x))
  where
    tag = maybe mempty signTag mbTag

-- | Smart constructor for 'Signed' data type with proper signing.
mkSigned :: (HasCryptoConfiguration, BiEnc a) => SignTag -> SecretKey -> a -> Signed a
mkSigned t sk x = Signed x (sign t sk x)

----------------------------------------------------------------------------
-- Proxy signing
----------------------------------------------------------------------------

-- | Formatter for 'ProxyCert' to show it in hex.
fullProxyCertHexF :: Format r (ProxyCert a -> r)
fullProxyCertHexF = later $ \(ProxyCert x) ->
    B16.formatBase16 . CC.unXSignature $ x

-- | Parse 'ProxyCert' from base16 encoded string.
parseFullProxyCert :: Text -> Either Text (ProxyCert a)
parseFullProxyCert s = do
    b <- B16.decode s
    ProxyCert <$> first fromString (CC.xsignature b)

-- | Make a proxy delegate signature with help of certificate. If the
-- delegate secret key passed doesn't pair with delegate public key in
-- certificate inside, we panic. Please check this condition outside
-- of this function.
proxySign
    :: (HasCryptoConfiguration, BiEnc a)
    => SignTag -> SecretKey -> ProxySecretKey w -> a -> ProxySignature w a
proxySign t sk@(SecretKey delegateSk) psk m
    | toPublic sk /= pskDelegatePk psk =
        error $ sformat ("proxySign called with irrelevant certificate "%
                         "(psk delegatePk: "%build%", real delegate pk: "%build%")")
                        (pskDelegatePk psk) (toPublic sk)
    | otherwise =
        UnsafeProxySignature
        { psigPsk = psk
        , psigSig = sigma
        }
  where
    PublicKey issuerPk = pskIssuerPk psk
    sigma =
        CC.sign emptyPass delegateSk $
        mconcat
            -- it's safe to put the tag after issuerPk because `CC.unXPub
            -- issuerPk` always takes 64 bytes
            ["01", CC.unXPub issuerPk, signTag t, Bi.serialize' m]

-- CHECK: @proxyVerify
-- | Verify delegated signature given issuer's pk, signature, message
-- space predicate and message itself.
proxyVerify
    :: (HasCryptoConfiguration, BiEnc w, BiEnc a)
    => SignTag -> ProxySignature w a -> (w -> Bool) -> a -> Bool
proxyVerify t psig omegaPred m =
    and [predCorrect, sigValid]
  where
    psk = psigPsk psig
    PublicKey issuerPk = pskIssuerPk psk
    PublicKey pdDelegatePkRaw = pskDelegatePk psk
    predCorrect = omegaPred (pskOmega psk)
    sigValid =
        CC.verify
            pdDelegatePkRaw
            (mconcat
                 [ "01"
                 , CC.unXPub issuerPk
                 , signTag t
                 , Bi.serialize' m
                 ])
            (psigSig psig)
