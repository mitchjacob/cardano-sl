{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE TypeOperators    #-}

-- | Bi typeclass and most basic functions.

module Pos.Binary.Class.Core
    ( BiEnc(..)
    , BiDec(..)
    , label
    , Bi
    , encodeBinary
    , decodeBinary
    , enforceSize
    , matchSize
    -- * CBOR re-exports
    , E.encodeListLen
    , D.decodeListLenCanonical
    , D.decodeListLenCanonicalOf
    , E.Encoding
    , D.Decoder
    , CBOR.Read.deserialiseIncremental
    , CBOR.Write.toLazyByteString
    , CBOR.Write.toBuilder
    , CBOR.Read.IDecode(..)
    -- * GHC-Generics-based encoding & decoding
    , genericEncode
    , genericDecode
    -- * Utils
    , toCborError
    , cborError
    ) where

import           Universum

import qualified Codec.CBOR.Decoding as D
import qualified Codec.CBOR.Encoding as E
import qualified Codec.CBOR.Read as CBOR.Read
import qualified Codec.CBOR.Write as CBOR.Write
import qualified Data.Binary as Binary
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BS.Lazy
import           Data.Fixed (Fixed (..), Nano)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Map as M
import qualified Data.Set as S
import           Data.Tagged (Tagged (..))
import qualified Data.Text as Text
import           Data.Time.Units (Microsecond, Millisecond)
import           Data.Typeable (typeRep)
import qualified Data.Vector as Vector
import qualified Data.Vector.Generic as Vector.Generic
import qualified GHC.Generics as G
import           Serokell.Data.Memory.Units (Byte, fromBytes, toBytes)

-- | This function must match the one from 'Pos.Util.Util'. It is copied here
-- to avoid a dependency and facilitate parallel builds.
toCborError :: Either Text a -> D.Decoder s a
toCborError = either (fail . toString) return

cborError :: Text -> D.Decoder s a
cborError = toCborError . Left

encodeBinary :: Binary.Binary a => a -> E.Encoding
encodeBinary = encode . BS.Lazy.toStrict . Binary.encode

decodeBinary :: Binary.Binary a => D.Decoder s a
decodeBinary = do
    x <- decode @ByteString
    toCborError $ case Binary.decodeOrFail (BS.Lazy.fromStrict x) of
        Left (_, _, err) -> Left (fromString err)
        Right (bs, _, res)
            | BS.Lazy.null bs -> Right res
            | otherwise       -> Left "decodeBinary: unconsumed input"

-- | Enforces that the input size is the same as the decoded one, failing in
-- case it's not.
enforceSize :: Text -> Int -> D.Decoder s ()
enforceSize lbl requestedSize = D.decodeListLenCanonical >>= matchSize requestedSize lbl

-- | Compare two sizes, failing if they are not equal.
matchSize :: Int -> Text -> Int -> D.Decoder s ()
matchSize requestedSize lbl actualSize =
  when (actualSize /= requestedSize) $
    cborError (lbl <> " failed the size check. Expected " <> show requestedSize <> ", found " <> show actualSize)

----------------------------------------

label :: Typeable a => Proxy a -> Text
label = show . typeRep

class Typeable a => BiEnc a where
    encode :: a -> E.Encoding

    encodeList :: [a] -> E.Encoding
    encodeList = defaultEncodeList

class Typeable a => BiDec a where
    decode :: D.Decoder s a

    decodeList :: D.Decoder s [a]
    decodeList = defaultDecodeList

type Bi a = (BiEnc a, BiDec a)

-- | Default @'E.Encoding'@ for list types.
defaultEncodeList :: BiEnc a => [a] -> E.Encoding
defaultEncodeList xs = E.encodeListLenIndef
                    <> Universum.foldr (\x r -> encode x <> r) E.encodeBreak xs

-- | Default @'D.Decoder'@ for list types.
defaultDecodeList :: BiDec a => D.Decoder s [a]
defaultDecodeList = do
    D.decodeListLenIndef
    D.decodeSequenceLenIndef (flip (:)) [] reverse decode

----------------------------------------------------------------------------
-- Primitive types
----------------------------------------------------------------------------

instance BiEnc () where
    encode = const E.encodeNull
instance BiDec () where
    decode = D.decodeNull

instance BiEnc Bool where
    encode = E.encodeBool
instance BiDec Bool where
    decode = D.decodeBool

instance BiEnc Char where
    encode c = E.encodeString (Text.singleton c)
    -- For [Char]/String we have a special encoding
    encodeList cs = E.encodeString (toText cs)
instance BiDec Char where
    decode = do t <- D.decodeString
                toCborError $ if Text.length t == 1
                  then Right (Text.head t)
                  else Left "expected a single char, found a string"

    decodeList    = do txt <- D.decodeString
                       return (toString txt) -- unpack lazily

----------------------------------------------------------------------------
-- Numeric data
----------------------------------------------------------------------------

instance BiEnc Integer where
    encode = E.encodeInteger
instance BiDec Integer where
    decode = D.decodeIntegerCanonical

instance BiEnc Word where
    encode = E.encodeWord
instance BiDec Word where
    decode = D.decodeWordCanonical

instance BiEnc Word8 where
    encode = E.encodeWord8
instance BiDec Word8 where
    decode = D.decodeWord8Canonical

instance BiEnc Word16 where
    encode = E.encodeWord16
instance BiDec Word16 where
    decode = D.decodeWord16Canonical

instance BiEnc Word32 where
    encode = E.encodeWord32
instance BiDec Word32 where
    decode = D.decodeWord32Canonical

instance BiEnc Word64 where
    encode = E.encodeWord64
instance BiDec Word64 where
    decode = D.decodeWord64Canonical

instance BiEnc Int where
    encode = E.encodeInt
instance BiDec Int where
    decode = D.decodeIntCanonical

instance BiEnc Float where
    encode = E.encodeFloat
instance BiDec Float where
    decode = D.decodeFloatCanonical

instance BiEnc Int32 where
    encode = E.encodeInt32
instance BiDec Int32 where
    decode = D.decodeInt32Canonical

instance BiEnc Int64 where
    encode = E.encodeInt64
instance BiDec Int64 where
    decode = D.decodeInt64Canonical

instance BiEnc Nano where
    encode (MkFixed resolution) = encode resolution
instance BiDec Nano where
    decode = MkFixed <$> decode

instance BiEnc Void where
    encode = absurd
instance BiDec Void where
    decode = fail "instance Bi Void: you shouldn't try to deserialize Void"

----------------------------------------------------------------------------
-- Tagged
----------------------------------------------------------------------------

instance (Typeable s, BiEnc a) => BiEnc (Tagged s a) where
    encode (Tagged a) = encode a
instance (Typeable s, BiDec a) => BiDec (Tagged s a) where
    decode = Tagged <$> decode

----------------------------------------------------------------------------
-- Containers
----------------------------------------------------------------------------

instance (BiEnc a, BiEnc b) => BiEnc (a,b) where
    encode (a,b) = E.encodeListLen 2
                <> encode a
                <> encode b
instance (BiDec a, BiDec b) => BiDec (a,b) where
    decode = do D.decodeListLenCanonicalOf 2
                !x <- decode
                !y <- decode
                return (x, y)

instance (BiEnc a, BiEnc b, BiEnc c) => BiEnc (a,b,c) where
    encode (a,b,c) = E.encodeListLen 3
                  <> encode a
                  <> encode b
                  <> encode c
instance (BiDec a, BiDec b, BiDec c) => BiDec (a,b,c) where
    decode = do D.decodeListLenCanonicalOf 3
                !x <- decode
                !y <- decode
                !z <- decode
                return (x, y, z)

instance (BiEnc a, BiEnc b, BiEnc c, BiEnc d) => BiEnc (a,b,c,d) where
    encode (a,b,c,d) = E.encodeListLen 4
                    <> encode a
                    <> encode b
                    <> encode c
                    <> encode d
instance (BiDec a, BiDec b, BiDec c, BiDec d) => BiDec (a,b,c,d) where
    decode = do D.decodeListLenCanonicalOf 4
                !a <- decode
                !b <- decode
                !c <- decode
                !d <- decode
                return (a, b, c, d)

instance BiEnc BS.ByteString where
    encode = E.encodeBytes
instance BiDec BS.ByteString where
    decode = D.decodeBytes

instance BiEnc Text.Text where
    encode = E.encodeString
instance BiDec Text.Text where
    decode = D.decodeString

instance BiEnc BS.Lazy.ByteString where
    encode = encode . BS.Lazy.toStrict
instance BiDec BS.Lazy.ByteString where
    decode = BS.Lazy.fromStrict <$> decode

instance BiEnc a => BiEnc [a] where
    encode = encodeList
instance BiDec a => BiDec [a] where
    decode = decodeList

instance (BiEnc a, BiEnc b) => BiEnc (Either a b) where
    encode (Left  x) = E.encodeListLen 2 <> E.encodeWord 0 <> encode x
    encode (Right x) = E.encodeListLen 2 <> E.encodeWord 1 <> encode x
instance (BiDec a, BiDec b) => BiDec (Either a b) where
    decode = do D.decodeListLenCanonicalOf 2
                t <- D.decodeWordCanonical
                case t of
                  0 -> do !x <- decode
                          return (Left x)
                  1 -> do !x <- decode
                          return (Right x)
                  _ -> cborError $ "decode@Either: unknown tag " <> show t

instance BiEnc a => BiEnc (NonEmpty a) where
    encode = defaultEncodeList . toList
instance BiDec a => BiDec (NonEmpty a) where
    decode =
        nonEmpty <$> defaultDecodeList >>= toCborError . \case
            Nothing -> Left "Expected a NonEmpty list, but an empty list was found!"
            Just xs -> Right xs

instance BiEnc a => BiEnc (Maybe a) where
    encode Nothing  = E.encodeListLen 0
    encode (Just x) = E.encodeListLen 1 <> encode x
instance BiDec a => BiDec (Maybe a) where
    decode = do n <- D.decodeListLenCanonical
                case n of
                  0 -> return Nothing
                  1 -> do !x <- decode
                          return (Just x)
                  _ -> cborError $ "decode@Maybe: unknown tag " <> show n

encodeContainerSkel :: (Word -> E.Encoding)
                    -> (container -> Int)
                    -> (accumFunc -> E.Encoding -> container -> E.Encoding)
                    -> accumFunc
                    -> container
                    -> E.Encoding
encodeContainerSkel encodeLen size foldFunction f  c =
    encodeLen (fromIntegral (size c)) <> foldFunction f mempty c
{-# INLINE encodeContainerSkel #-}

decodeContainerSkelWithReplicate
  :: BiDec a
  => D.Decoder s Int
     -- ^ How to get the size of the container
  -> (Int -> D.Decoder s a -> D.Decoder s container)
     -- ^ replicateM for the container
  -> ([container] -> container)
     -- ^ concat for the container
  -> D.Decoder s container
decodeContainerSkelWithReplicate decodeLen replicateFun fromList = do
    -- Look at how much data we have at the moment and use it as the limit for
    -- the size of a single call to replicateFun. We don't want to use
    -- replicateFun directly on the result of decodeLen since this might lead to
    -- DOS attack (attacker providing a huge value for length). So if it's above
    -- our limit, we'll do manual chunking and then combine the containers into
    -- one.
    size <- decodeLen
    limit <- D.peekAvailable
    if size <= limit
       then replicateFun size decode
       else do
           -- Take the max of limit and a fixed chunk size (note: limit can be
           -- 0). This basically means that the attacker can make us allocate a
           -- container of size 128 even though there's no actual input.
           let chunkSize = max limit 128
               (d, m) = size `divMod` chunkSize
               buildOne s = replicateFun s decode
           containers <- sequence $ buildOne m : replicate d (buildOne chunkSize)
           return $! fromList containers
{-# INLINE decodeContainerSkelWithReplicate #-}

encodeMapSkel :: (BiEnc k, BiEnc v)
              => (m -> Int)
              -> ((k -> v -> E.Encoding -> E.Encoding) -> E.Encoding -> m -> E.Encoding)
              -> m
              -> E.Encoding
encodeMapSkel size foldrWithKey =
  encodeContainerSkel
    E.encodeMapLen
    size
    foldrWithKey
    (\k v b -> encode k <> encode v <> b)
{-# INLINE encodeMapSkel #-}

-- | Checks canonicity by comparing the new key being decoded with
-- the previous one, to enfore these are sorted the correct way.
-- See: https://tools.ietf.org/html/rfc7049#section-3.9
-- "[..]The keys in every map must be sorted lowest value to highest.[...]"
decodeMapSkel :: (Ord k, BiDec k, BiDec v) => ([(k,v)] -> m) -> D.Decoder s m
decodeMapSkel fromDistinctAscList = do
    n <- D.decodeMapLenCanonical
    case n of
        0 -> return (fromDistinctAscList [])
        _ -> do
            (firstKey, firstValue) <- decodeEntry
            fromDistinctAscList <$> decodeEntries (n - 1) firstKey [(firstKey, firstValue)]
  where
    -- Decode a single (k,v).
    decodeEntry :: (BiDec k, BiDec v) => D.Decoder s (k,v)
    decodeEntry = do
        !k <- decode
        !v <- decode
        return (k, v)

    -- Decode all the entries, enforcing canonicity by ensuring that the
    -- previous key is smaller than the next one.
    decodeEntries :: (BiDec k, BiDec v, Ord k) => Int -> k -> [(k,v)] -> D.Decoder s [(k,v)]
    decodeEntries 0 _ acc = pure $ reverse acc
    decodeEntries !remainingPairs previousKey !acc = do
        p@(newKey, _) <- decodeEntry
        -- Order of keys needs to be strictly increasing, because otherwise it's
        -- possible to supply lists with various amount of duplicate keys which
        -- will result in the same map as long as the last value of the given
        -- key on the list is the same in all of them.
        case newKey > previousKey of
            True  -> decodeEntries (remainingPairs - 1) newKey (p : acc)
            False -> cborError "Canonicity violation whilst decoding a Map!"
{-# INLINE decodeMapSkel #-}

instance (Hashable k, Ord k, BiEnc k, BiEnc v) => BiEnc (HM.HashMap k v) where
    encode = encodeMapSkel HM.size $ \f acc ->
        -- We need to encode the list with keys sorted in ascending order as
        -- that's the only representation we accept during decoding.
        foldr (uncurry f) acc . sortWith fst . HM.toList
instance (Hashable k, Ord k, BiDec k, BiDec v) => BiDec (HM.HashMap k v) where
    decode = decodeMapSkel HM.fromList

instance (Ord k, BiEnc k, BiEnc v) => BiEnc (Map k v) where
    encode = encodeMapSkel M.size M.foldrWithKey
instance (Ord k, BiDec k, BiDec v) => BiDec (Map k v) where
    decode = decodeMapSkel M.fromDistinctAscList

encodeSetSkel :: BiEnc a
              => (s -> Int)
              -> ((a -> E.Encoding -> E.Encoding) -> E.Encoding -> s -> E.Encoding)
              -> s
              -> E.Encoding
encodeSetSkel size foldFunction =
    mappend encodeSetTag .
    encodeContainerSkel E.encodeListLen size foldFunction (\a b -> encode a <> b)
{-# INLINE encodeSetSkel #-}

-- We stitch a `258` in from of a (Hash)Set, so that tools which
-- programmatically check for canonicity can recognise it from a normal
-- array. Why 258? This will be formalised pretty soon, but IANA allocated
-- 256...18446744073709551615 to "First come, first served":
-- https://www.iana.org/assignments/cbor-tags/cbor-tags.xhtml Currently `258` is
-- the first unassigned tag and as it requires 2 bytes to be encoded, it sounds
-- like the best fit.
setTag :: Word
setTag = 258

encodeSetTag :: E.Encoding
encodeSetTag = E.encodeTag setTag

decodeSetTag :: D.Decoder s ()
decodeSetTag = do
    t <- D.decodeTagCanonical
    when (t /= setTag) $ cborError ("decodeSetTag: this doesn't appear to be a Set. Found tag: " <> show t)

decodeSetSkel :: (Ord a, BiDec a) => ([a] -> c) -> D.Decoder s c
decodeSetSkel fromDistinctAscList = do
    decodeSetTag
    n <- D.decodeListLenCanonical
    case n of
        0 -> return (fromDistinctAscList [])
        _ -> do
            firstValue <- decode
            fromDistinctAscList <$> decodeEntries (n - 1) firstValue [firstValue]
  where
    decodeEntries :: (BiDec v, Ord v) => Int -> v -> [v] -> D.Decoder s [v]
    decodeEntries 0 _ acc = pure $ reverse acc
    decodeEntries !remainingEntries previousValue !acc = do
        newValue <- decode
        -- Order of values needs to be strictly increasing, because otherwise
        -- it's possible to supply lists with various amount of duplicates which
        -- will result in the same set.
        case newValue > previousValue of
            True  -> decodeEntries (remainingEntries - 1) newValue (newValue : acc)
            False -> cborError "Canonicity violation whilst decoding a Set!"
{-# INLINE decodeSetSkel #-}

instance (Hashable a, Ord a, BiEnc a) => BiEnc (HashSet a) where
    encode = encodeSetSkel HS.size $ \f acc ->
        -- We need to encode the list sorted in ascending order as that's the only
        -- representation we accept during decoding.
        foldr f acc . sort . HS.toList
instance (Hashable a, Ord a, BiDec a) => BiDec (HashSet a) where
    decode = decodeSetSkel HS.fromList

instance (Ord a, BiEnc a) => BiEnc (Set a) where
    encode = encodeSetSkel S.size S.foldr
instance (Ord a, BiDec a) => BiDec (Set a) where
    decode = decodeSetSkel S.fromDistinctAscList

-- | Generic encoder for vectors. Its intended use is to allow easy
-- definition of 'Serialise' instances for custom vector
encodeVector :: (BiEnc a, Vector.Generic.Vector v a)
             => v a -> E.Encoding
encodeVector = encodeContainerSkel
    E.encodeListLen
    Vector.Generic.length
    Vector.Generic.foldr
    (\a b -> encode a <> b)
{-# INLINE encodeVector #-}

-- | Generic decoder for vectors. Its intended use is to allow easy
-- definition of 'Serialise' instances for custom vector
decodeVector :: (BiDec a, Vector.Generic.Vector v a)
             => D.Decoder s (v a)
decodeVector = decodeContainerSkelWithReplicate
    D.decodeListLenCanonical
    Vector.Generic.replicateM
    Vector.Generic.concat
{-# INLINE decodeVector #-}

instance (BiEnc a) => BiEnc (Vector.Vector a) where
  encode = encodeVector
  {-# INLINE encode #-}
instance (BiDec a) => BiDec (Vector.Vector a) where
  decode = decodeVector
  {-# INLINE decode #-}

----------------------------------------------------------------------------
-- Other types
----------------------------------------------------------------------------

instance BiEnc Millisecond where
    encode = encode . toInteger
instance BiDec Millisecond where
    decode = fromInteger <$> decode

instance BiEnc Microsecond where
    encode = encode . toInteger
instance BiDec Microsecond where
    decode = fromInteger <$> decode

instance BiEnc Byte where
    encode = encode . toBytes
instance BiDec Byte where
    decode = fromBytes <$> decode

----------------------------------------------------------------------------
-- Generic deriving
----------------------------------------------------------------------------

genericEncode :: (Generic a, GSerialiseEncode (G.Rep a)) => a -> E.Encoding
genericEncode = gencode . G.from

genericDecode :: (Generic a, GSerialiseDecode (G.Rep a)) => D.Decoder s a
genericDecode = G.to <$> gdecode

class GSerialiseEncode f where
    gencode  :: f a -> E.Encoding

class GSerialiseDecode f where
    gdecode  :: D.Decoder s (f a)

instance GSerialiseEncode G.V1 where
    -- Data types without constructors are still serialised as null value
    gencode _ = E.encodeNull

instance GSerialiseDecode G.V1 where
    gdecode   = error "G.V1 don't have contructors" <$ D.decodeNull

instance GSerialiseEncode G.U1 where
    -- Constructors without fields are serialised as null value
    gencode _ = E.encodeListLen 0

instance GSerialiseDecode G.U1 where
    gdecode   = do
      n <- D.decodeListLenCanonical
      when (n /= 0) $ cborError "expect list of length 0"
      return G.U1

instance GSerialiseEncode a => GSerialiseEncode (G.M1 i c a) where
    -- Metadata (constructor name, etc) is skipped
    gencode = gencode . G.unM1

instance GSerialiseDecode a => GSerialiseDecode (G.M1 i c a) where
    gdecode = G.M1 <$> gdecode

instance BiEnc a => GSerialiseEncode (G.K1 i a) where
    -- Constructor field (Could only appear in one-field & one-constructor
    -- data types). In all other cases we go through GSerialise{Sum,Prod}
    gencode (G.K1 a) = E.encodeListLen 1
                     <> encode a

instance BiDec a => GSerialiseDecode (G.K1 i a) where
    gdecode = do
      n <- D.decodeListLenCanonical
      when (n /= 1) $
        cborError "expect list of length 1"
      G.K1 <$> decode

instance (GSerialiseProdEnc f, GSerialiseProdEnc g) => GSerialiseEncode (f G.:*: g) where
    -- Products are serialised as N-tuples with 0 constructor tag
    gencode (f G.:*: g)
        = E.encodeListLen (nFields (Proxy :: Proxy (f G.:*: g)))
       <> encodeSeq f
       <> encodeSeq g

instance (GSerialiseProdDec f, GSerialiseProdDec g) => GSerialiseDecode (f G.:*: g) where
    gdecode = do
      let nF = nFields (Proxy :: Proxy (f G.:*: g))
      n <- D.decodeListLenCanonical
      -- TODO FIXME: signedness of list length
      when (fromIntegral n /= nF) $
        cborError $ "Wrong number of fields: expected="<>show (nF)<>" got="<>show n
      !f <- gdecodeSeq
      !g <- gdecodeSeq
      return $ f G.:*: g

instance (GSerialiseSumEnc f, GSerialiseSumEnc g) => GSerialiseEncode (f G.:+: g) where
    -- Sum types are serialised as N-tuples and first element is
    -- constructor tag
    gencode a = E.encodeListLen (numOfFields a + 1)
             <> encode (conNumber a)
             <> encodeSum a

instance (GSerialiseSumDec f, GSerialiseSumDec g) => GSerialiseDecode (f G.:+: g) where
    gdecode = do
        n <- D.decodeListLenCanonical
        -- TODO FIXME: Again signedness
        when (n == 0) $
          cborError "Empty list encountered for sum type"
        nCon  <- D.decodeWordCanonical
        trueN <- fieldsForCon (Proxy :: Proxy (f G.:+: g)) nCon
        when (n-1 /= fromIntegral trueN ) $
          cborError $ "Number of fields mismatch: expected="<>show trueN<>" got="<>show n
        decodeSum nCon



-- | Serialization of product types
class GSerialiseProdBase (f :: * -> *) where
    -- | Number of fields in product type
    nFields   :: Proxy f -> Word

class GSerialiseProdBase f => GSerialiseProdEnc f where
    -- | Encode fields sequentially without writing header
    encodeSeq :: f a -> E.Encoding

class GSerialiseProdBase f => GSerialiseProdDec f where
    -- | Decode fields sequentially without reading header
    gdecodeSeq :: D.Decoder s (f a)

instance (GSerialiseProdBase f, GSerialiseProdBase g) =>
         GSerialiseProdBase (f G.:*: g) where
    nFields _ = nFields (Proxy :: Proxy f) + nFields (Proxy :: Proxy g)

instance (GSerialiseProdEnc f, GSerialiseProdEnc g) =>
         GSerialiseProdEnc (f G.:*: g) where
    encodeSeq (f G.:*: g) = encodeSeq f <> encodeSeq g

instance (GSerialiseProdDec f, GSerialiseProdDec g) =>
         GSerialiseProdDec (f G.:*: g) where
    gdecodeSeq = do
        !f <- gdecodeSeq
        !g <- gdecodeSeq
        return (f G.:*: g)

instance GSerialiseProdBase G.U1 where
    -- N.B. Could only be reached when one of constructors in sum type
    --      don't have parameters
    nFields   _ = 0
instance GSerialiseProdEnc G.U1 where
    encodeSeq _ = mempty
instance GSerialiseProdDec G.U1 where
    gdecodeSeq  = return G.U1

instance GSerialiseProdBase (G.K1 i a) where
    -- Ordinary field
    nFields    _     = 1
instance BiEnc a => GSerialiseProdEnc (G.K1 i a) where
    encodeSeq (G.K1 f) = encode f
instance BiDec a => GSerialiseProdDec (G.K1 i a) where
    gdecodeSeq       = G.K1 <$> decode

instance (i ~ G.S, GSerialiseProdBase f) => GSerialiseProdBase (G.M1 i c f) where
    -- We skip metadata
    nFields     _     = 1
instance (i ~ G.S, GSerialiseProdEnc f) => GSerialiseProdEnc (G.M1 i c f) where
    encodeSeq  (G.M1 f) = encodeSeq f
instance (i ~ G.S, GSerialiseProdDec f) => GSerialiseProdDec (G.M1 i c f) where
    gdecodeSeq        = G.M1 <$> gdecodeSeq



-- | Serialization of sum types
--
class GSerialiseSumBase f where
    -- | Number of constructor of given value
    conNumber   :: f a -> Word
    -- | Number of fields of given value
    numOfFields :: f a -> Word
    -- | Number of constructors
    nConstructors :: Proxy f -> Word
    -- | Number of fields for given constructor number
    fieldsForCon  :: Proxy f -> Word -> D.Decoder s Word

class GSerialiseSumBase f => GSerialiseSumEnc f where
    -- | Encode field
    encodeSum   :: f a  -> E.Encoding

class GSerialiseSumBase f => GSerialiseSumDec f where
    -- | Decode field
    decodeSum     :: Word -> D.Decoder s (f a)

instance (GSerialiseSumBase f, GSerialiseSumBase g) => GSerialiseSumBase (f G.:+: g) where
    conNumber x = case x of
      G.L1 f -> conNumber f
      G.R1 g -> conNumber g + nConstructors (Proxy :: Proxy f)
    numOfFields x = case x of
      G.L1 f -> numOfFields f
      G.R1 g -> numOfFields g
    nConstructors _ = nConstructors (Proxy :: Proxy f)
                    + nConstructors (Proxy :: Proxy g)

    fieldsForCon _ n | n < nL    = fieldsForCon (Proxy :: Proxy f) n
                     | otherwise = fieldsForCon (Proxy :: Proxy g) (n - nL)
      where
        nL = nConstructors (Proxy :: Proxy f)

instance (GSerialiseSumEnc f, GSerialiseSumEnc g) => GSerialiseSumEnc (f G.:+: g) where
    encodeSum x = case x of
      G.L1 f -> encodeSum f
      G.R1 g -> encodeSum g

instance (GSerialiseSumDec f, GSerialiseSumDec g) => GSerialiseSumDec (f G.:+: g) where
    decodeSum nCon | nCon < nL = G.L1 <$> decodeSum nCon
                   | otherwise = G.R1 <$> decodeSum (nCon - nL)
      where
        nL = nConstructors (Proxy :: Proxy f)

instance (i ~ G.C, GSerialiseProdBase f) => GSerialiseSumBase (G.M1 i c f) where
    conNumber    _   = 0
    numOfFields  _   = nFields (Proxy :: Proxy f)
    nConstructors  _ = 1
    fieldsForCon _ 0 = return $ nFields (Proxy :: Proxy f)
    fieldsForCon _ _ = cborError "Bad constructor number"

instance (i ~ G.C, GSerialiseProdEnc f) => GSerialiseSumEnc (G.M1 i c f) where
    encodeSum   (G.M1 f) = encodeSeq f

instance (i ~ G.C, GSerialiseProdDec f) => GSerialiseSumDec (G.M1 i c f) where
    decodeSum      0 = G.M1 <$> gdecodeSeq
    decodeSum      _ = cborError "bad constructor number"
