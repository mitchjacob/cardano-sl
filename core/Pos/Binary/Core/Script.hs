{-# LANGUAGE TemplateHaskell #-}
module Pos.Binary.Core.Script () where

import           Universum

import           Data.Hashable (Hashable, hashWithSalt)
import qualified PlutusCore.Program as PLCore
import qualified PlutusCore.Term as PLCore
import qualified PlutusTypes.ConSig as PLTypes
import qualified PlutusTypes.Type as PLTypes
import qualified Utils.ABT as ABT
import qualified Utils.Names as Names
import qualified Utils.Vars as Vars

import           Pos.Binary.Class (BiDec (..), BiEnc (..), Cons (..), Field (..), deriveSimpleBi,
                                   genericDecode, genericEncode, serialize')
import           Pos.Core.Common (Script (..), ScriptVersion)
import           Pos.Core.Script ()


instance BiEnc Vars.FreeVar where
    encode = genericEncode
instance BiDec Vars.FreeVar where
    decode = genericDecode

instance BiEnc Vars.MetaVar where
    encode = genericEncode
instance BiDec Vars.MetaVar where
    decode = genericDecode

instance BiEnc Vars.BoundVar where
    encode = genericEncode
instance BiDec Vars.BoundVar where
    decode = genericDecode

instance BiEnc PLTypes.TyConSig where
    encode = genericEncode
instance BiDec PLTypes.TyConSig where
    decode = genericDecode

instance BiEnc PLTypes.ConSig where
    encode = genericEncode
instance BiDec PLTypes.ConSig where
    decode = genericDecode

instance BiEnc a => BiEnc (Names.Sourced a) where
    encode = genericEncode
instance BiDec a => BiDec (Names.Sourced a) where
    decode = genericDecode

instance BiEnc ABT.Variable where
    encode = genericEncode
instance BiDec ABT.Variable where
    decode = genericDecode

instance (Typeable f, BiEnc (f (ABT.Scope f))) => BiEnc (ABT.ABT f) where
    encode = genericEncode
instance (Typeable f, BiDec (f (ABT.Scope f))) => BiDec (ABT.ABT f) where
    decode = genericDecode

instance (Typeable f, BiEnc (f (ABT.Scope f))) => BiEnc (ABT.Scope f) where
    encode = genericEncode
instance (Typeable f, BiDec (f (ABT.Scope f))) => BiDec (ABT.Scope f) where
    decode = genericDecode

instance (Typeable r, BiEnc r) => BiEnc (PLCore.ClauseF r) where
    encode = genericEncode
instance (Typeable r, BiDec r) => BiDec (PLCore.ClauseF r) where
    decode = genericDecode

instance BiEnc a => BiEnc (PLCore.TermF a) where
    encode = genericEncode
instance BiDec a => BiDec (PLCore.TermF a) where
    decode = genericDecode

instance BiEnc a => BiEnc (PLTypes.TypeF a) where
    encode = genericEncode
instance BiDec a => BiDec (PLTypes.TypeF a) where
    decode = genericDecode

instance BiEnc PLCore.SimplePattern where
    encode = genericEncode
instance BiDec PLCore.SimplePattern where
    decode = genericDecode

instance BiEnc PLCore.PrimData where
    encode = genericEncode
instance BiDec PLCore.PrimData where
    decode = genericDecode

instance BiEnc PLCore.Program where
    encode = genericEncode
instance BiDec PLCore.Program where
    decode = genericDecode

deriveSimpleBi ''Script [
    Cons 'Script [
        Field [| scrVersion :: ScriptVersion |],
        Field [| scrScript  :: ByteString   |]
    ]]

instance Hashable PLCore.Term where
    hashWithSalt s = hashWithSalt s . serialize'

instance Hashable PLCore.Program where
    hashWithSalt s = hashWithSalt s . serialize'
