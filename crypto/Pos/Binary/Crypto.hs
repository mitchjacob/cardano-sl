{-# LANGUAGE CPP            #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ViewPatterns   #-}

-- | Serializable instances for Pos.Crypto.*

module Pos.Binary.Crypto () where

import           Universum

import qualified Cardano.Crypto.Wallet as CC
import           Control.Lens (_Left)
import           Crypto.Hash (digestFromByteString)
import qualified Crypto.Math.Edwards25519 as Ed25519
import qualified Crypto.PVSS as Pvss
import qualified Crypto.SCRAPE as Scrape
import qualified Crypto.Sign.Ed25519 as EdStandard
import qualified Data.ByteArray as ByteArray
import qualified Data.ByteString as BS
import           Formatting (int, sformat, (%))

import           Pos.Binary.Class (AsBinary (..), Bi, BiDec (..), BiEnc (..), Cons (..), Field (..),
                                   decodeBinary, deriveSimpleBi, encodeBinary, encodeListLen,
                                   enforceSize)
import           Pos.Crypto.AsBinary (decShareBytes, encShareBytes, secretBytes, vssPublicKeyBytes)
import           Pos.Crypto.Hashing (AbstractHash (..), HashAlgorithm, WithHash (..), withHash)
import           Pos.Crypto.HD (HDAddressPayload (..))
import           Pos.Crypto.Scrypt (EncryptedPass (..))
import qualified Pos.Crypto.SecretSharing as C
import           Pos.Crypto.Signing.Types (ProxyCert (..), ProxySecretKey (..), ProxySignature (..),
                                           PublicKey (..), SecretKey (..), Signature (..),
                                           Signed (..))
import           Pos.Crypto.Signing.Types.Redeem (RedeemPublicKey (..), RedeemSecretKey (..),
                                                  RedeemSignature (..))
import           Pos.Crypto.Signing.Types.Safe (EncryptedSecretKey (..), PassPhrase,
                                                passphraseLength)
import           Pos.Util.Util (cborError, toCborError)
import           Pos.Util.Verification (Unver, mkUnver)

instance Bi a => BiEnc (WithHash a) where
    encode = encode . whData
instance Bi a => BiDec (WithHash a) where
    decode = withHash <$> decode

----------------------------------------------------------------------------
-- Hashing
----------------------------------------------------------------------------

instance (Typeable algo, Typeable a, HashAlgorithm algo) => BiEnc (AbstractHash algo a) where
    encode (AbstractHash digest) = encode (ByteArray.convert digest :: BS.ByteString)
instance (Typeable algo, Typeable a, HashAlgorithm algo) => BiDec (AbstractHash algo a) where
    -- FIXME bad decode: it reads an arbitrary-length byte string.
    -- Better instance: know the hash algorithm up front, read exactly that
    -- many bytes, fail otherwise. Then convert to a digest.
    decode = do
        bs <- decode @ByteString
        toCborError $ case digestFromByteString bs of
            Nothing -> Left "AbstractHash.decode: invalid digest"
            Just x  -> Right (AbstractHash x)

----------------------------------------------------------------------------
-- SecretSharing
----------------------------------------------------------------------------

instance BiEnc Scrape.PublicKey where
    encode = encodeBinary
instance BiDec Scrape.PublicKey where
    decode = decodeBinary

deriving instance BiEnc C.VssPublicKey
deriving instance BiDec C.VssPublicKey

instance BiEnc Scrape.KeyPair where
    encode = encodeBinary
instance BiDec Scrape.KeyPair where
    decode = decodeBinary

deriving instance BiEnc C.VssKeyPair
deriving instance BiDec C.VssKeyPair

instance BiEnc Scrape.Secret where
    encode = encodeBinary
instance BiDec Scrape.Secret where
    decode = decodeBinary

deriving instance BiEnc C.Secret
deriving instance BiDec C.Secret

instance BiEnc Scrape.DecryptedShare where
    encode = encodeBinary
instance BiDec Scrape.DecryptedShare where
    decode = decodeBinary

deriving instance BiEnc C.DecShare
deriving instance BiDec C.DecShare

instance BiEnc Scrape.EncryptedSi where
    encode = encodeBinary
instance BiDec Scrape.EncryptedSi where
    decode = decodeBinary

deriving instance BiEnc C.EncShare
deriving instance BiDec C.EncShare

instance BiEnc Scrape.ExtraGen where
    encode = encodeBinary
instance BiDec Scrape.ExtraGen where
    decode = decodeBinary

instance BiEnc Scrape.Commitment where
    encode = encodeBinary
instance BiDec Scrape.Commitment where
    decode = decodeBinary

instance BiEnc Scrape.Proof where
    encode = encodeBinary
instance BiDec Scrape.Proof where
    decode = decodeBinary

instance BiEnc Scrape.ParallelProofs where
    encode = encodeBinary
instance BiDec Scrape.ParallelProofs where
    decode = decodeBinary

deriveSimpleBi ''C.SecretProof [
    Cons 'C.SecretProof [
        Field [| C.spExtraGen       :: Scrape.ExtraGen       |],
        Field [| C.spProof          :: Scrape.Proof          |],
        Field [| C.spParallelProofs :: Scrape.ParallelProofs |],
        Field [| C.spCommitments    :: [Scrape.Commitment]   |]
    ]
  ]

----------------------------------------------------------------------------
-- SecretSharing AsBinary
----------------------------------------------------------------------------

-- !A note about these instances! --
--
-- For most of the secret sharing types the only check we do during
-- deserialization is length check. As long as length matches our
-- expectations, the decoding succeeds (look at 'Binary' instances in
-- 'pvss') which in turn means that we can use 'fromBinary' and be quite
-- sure it will succeed. That's why it's important to check length here
-- (this check is cheap, so it's good to do it as soon as possible).
-- 'SecretProof' used to be an exception, but currently we don't use
-- 'AsBinary' for 'SecretProof' (we might in the future); this said, it's
-- alright to use 'AsBinary' for variable-length things as long as you're
-- careful.
--
#define BiMacro(B, BYTES) \
  instance BiEnc (AsBinary B) where { \
    encode (AsBinary bs) = encode bs }; \
  instance BiDec (AsBinary B) where { \
    decode = do { bs <- decode \
                ; when (BYTES /= length bs) (cborError "AsBinary B: length mismatch!") \
                ; return (AsBinary bs) } }; \

BiMacro(C.VssPublicKey, vssPublicKeyBytes)
BiMacro(C.Secret, secretBytes)
BiMacro(C.DecShare, decShareBytes)
BiMacro(C.EncShare, encShareBytes)

----------------------------------------------------------------------------
-- Signing
----------------------------------------------------------------------------

instance BiEnc Ed25519.PointCompressed where
    encode (Ed25519.unPointCompressed -> k) = encode k
instance BiDec Ed25519.PointCompressed where
    decode = Ed25519.pointCompressed <$> decode

instance BiEnc Ed25519.Scalar where
    encode (Ed25519.unScalar -> k) = encode k
instance BiDec Ed25519.Scalar where
    decode = Ed25519.scalar <$> decode

instance BiEnc Ed25519.Signature where
    encode (Ed25519.Signature s) = encode s
instance BiDec Ed25519.Signature where
    decode = Ed25519.Signature <$> decode

instance BiEnc CC.ChainCode where
    encode (CC.ChainCode c) = encode c
instance BiDec CC.ChainCode where
    decode = CC.ChainCode <$> decode

instance BiEnc CC.XPub where
    encode (CC.unXPub -> kc) = encode kc
instance BiDec CC.XPub where
    decode = toCborError . over _Left fromString . CC.xpub =<< decode

instance BiEnc CC.XPrv where
    encode (CC.unXPrv -> kc) = encode kc
instance BiDec CC.XPrv where
    decode = toCborError . over _Left fromString . CC.xprv =<< decode @ByteString

instance BiEnc CC.XSignature where
    encode (CC.unXSignature -> bs) = encode bs
instance BiDec CC.XSignature where
    decode = toCborError . over _Left fromString . CC.xsignature =<< decode

deriving instance Typeable a => BiEnc (Signature a)
deriving instance Typeable a => BiDec (Signature a)
deriving instance BiEnc PublicKey
deriving instance BiDec PublicKey
deriving instance BiEnc SecretKey
deriving instance BiDec SecretKey

instance BiEnc EncryptedSecretKey where
    encode (EncryptedSecretKey sk pph) = encodeListLen 2
                                      <> encode sk
                                      <> encode pph
instance BiDec EncryptedSecretKey where
    decode = EncryptedSecretKey
         <$  enforceSize "EncryptedSecretKey" 2
         <*> decode
         <*> decode

instance BiEnc a => BiEnc (Signed a) where
    encode (Signed v s) = encodeListLen 2
                       <> encode v
                       <> encode s
instance BiDec a => BiDec (Signed a) where
    decode = Signed
         <$  enforceSize "Signed" 2
         <*> decode
         <*> decode

deriving instance Typeable w => BiEnc (ProxyCert w)
deriving instance Typeable w => BiDec (ProxyCert w)

instance BiEnc w => BiEnc (ProxySecretKey w) where
    encode UnsafeProxySecretKey {..} =
        encodeListLen 4 <>
        encode pskOmega <>
        encode pskIssuerPk <>
        encode pskDelegatePk <>
        encode pskCert
instance BiDec w => BiDec (Unver (ProxySecretKey w)) where
    decode =
        fmap mkUnver $
        UnsafeProxySecretKey <$
        enforceSize "ProxySecretKey" 4 <*>
        decode <*>
        decode <*>
        decode <*>
        decode

instance (Typeable a, BiEnc w) => BiEnc (ProxySignature w a) where
    encode UnsafeProxySignature {..} =
        encodeListLen 2 <>
        encode psigPsk <>
        encode psigSig
instance (Typeable a, BiDec w) => BiDec (Unver (ProxySignature w a)) where
    decode = do
        enforceSize "ProxySignature" 2
        psigPsk <- decode
        psigSig <- decode
        pure $ UnsafeProxySignature <$> psigPsk <*> pure psigSig

instance BiEnc PassPhrase where
    encode pp = encode (ByteArray.convert pp :: ByteString)
instance BiDec PassPhrase where
    decode = do
        bs <- decode @ByteString
        let bl = BS.length bs
        -- Currently passphrase may be either 32-byte long or empty (for
        -- unencrypted keys).
        toCborError $ if bl == 0 || bl == passphraseLength
            then Right $ ByteArray.convert bs
            else Left $ sformat
                 ("put@PassPhrase: expected length 0 or "%int%", not "%int)
                 passphraseLength bl

instance BiEnc EncryptedPass where
    encode (EncryptedPass ep) = encode ep
instance BiDec EncryptedPass where
    decode = EncryptedPass <$> decode

-------------------------------------------------------------------------------
-- Hierarchical derivation
-------------------------------------------------------------------------------

instance BiEnc HDAddressPayload where
    encode (HDAddressPayload payload) = encode payload
instance BiDec HDAddressPayload where
    decode = HDAddressPayload <$> decode

-------------------------------------------------------------------------------
-- Standard Ed25519 instances for ADA redeem keys
-------------------------------------------------------------------------------

instance BiEnc EdStandard.PublicKey where
    encode (EdStandard.PublicKey k) = encode k
instance BiDec EdStandard.PublicKey where
    decode = EdStandard.PublicKey <$> decode

instance BiEnc EdStandard.SecretKey where
    encode (EdStandard.SecretKey k) = encode k
instance BiDec EdStandard.SecretKey where
    decode = EdStandard.SecretKey <$> decode

instance BiEnc EdStandard.Signature where
    encode (EdStandard.Signature s) = encode s
instance BiDec EdStandard.Signature where
    decode = EdStandard.Signature <$> decode

deriving instance BiEnc RedeemPublicKey
deriving instance BiDec RedeemPublicKey

deriving instance BiEnc RedeemSecretKey
deriving instance BiDec RedeemSecretKey

deriving instance Typeable a => BiEnc (RedeemSignature a)
deriving instance Typeable a => BiDec (RedeemSignature a)
