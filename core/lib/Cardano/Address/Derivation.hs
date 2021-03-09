{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_HADDOCK prune #-}

module Cardano.Address.Derivation
    (
    -- * Overview
    -- $overview

    -- * Key Derivation
    -- ** Types
      Index (getIndex)
    , hardenedIndex
    , softIndex
    , wholeDomainIndex
    , castWholeDomainIndex
    , Depth (..)
    , DerivationType (..)

    -- * Abstractions
    , GenMasterKey (..)
    , HardDerivation (..)
    , SoftDerivation (..)

    -- * Low-Level Cryptography Primitives
    -- ** XPrv
    , XPrv
    , xprvFromBytes
    , xprvToBytes
    , xprvPrivateKey
    , xprvChainCode
    , toXPub

    -- ** XPub
    , XPub
    , xpubFromBytes
    , xpubToBytes
    , xpubPublicKey
    , xpubChainCode

    -- ** XSignature
    , XSignature
    , sign
    , verify

    -- Internal / Not exposed by Haddock
    , DerivationScheme (..)
    , deriveXPrv
    , deriveXPub
    , generate
    , generateNew
    , hashCredential
    , credentialHashSize
    ------------------
    ) where

import Prelude

import Cardano.Crypto.Wallet
    ( DerivationScheme (..) )
import Cardano.Mnemonic
    ( SomeMnemonic )
import Control.DeepSeq
    ( NFData )
import Crypto.Error
    ( eitherCryptoError )
import Crypto.Hash
    ( hash )
import Crypto.Hash.Algorithms
    ( Blake2b_224 (..) )
import Crypto.Hash.IO
    ( HashAlgorithm (hashDigestSize) )
import Data.ByteArray
    ( ByteArrayAccess, ScrubbedBytes )
import Data.ByteString
    ( ByteString )
import Data.Either.Extra
    ( eitherToMaybe )
import Data.String
    ( fromString )
import Data.Word
    ( Word32 )
import Fmt
    ( Buildable (..) )
import GHC.Generics
    ( Generic )
import GHC.Stack
    ( HasCallStack )

import qualified Cardano.Crypto.Wallet as CC
import qualified Crypto.ECC.Edwards25519 as Ed25519
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS

-- $overview
--
-- These abstractions allow generating root private key, also called /Master Key/
-- and then basing on it enable address derivation

--
-- Low-Level Cryptography Primitives
--

-- | An opaque type representing an extended private key.
--
-- __Properties:__
--
-- ===== Roundtripping
--
-- @forall xprv. 'xprvFromBytes' ('xprvToBytes' xprv) == 'Just' xprv@
--
-- ===== Chain Code Invariance
--
-- @forall xprv. 'xprvChainCode' xprv == 'xpubChainCode' ('toXPub' xprv)@
--
-- ===== Public Key Signature
--
-- @forall xprv msg. 'verify' ('toXPub' xprv) msg ('sign' xprv msg) == 'True'@
--
-- @since 1.0.0
type XPrv = CC.XPrv

-- | An opaque type representing an extended public key.
--
-- __Properties:__
--
-- ===== Roundtripping
--
-- @forall xpub. 'xpubFromBytes' ('xpubToBytes' xpub) == 'Just' xpub@
--
-- @since 1.0.0
type XPub = CC.XPub

-- | An opaque type representing a signature made from an 'XPrv'.
--
-- @since 1.0.0
type XSignature = CC.XSignature

-- | Construct an 'XPub' from raw 'ByteString' (64 bytes).
--
-- @since 1.0.0
xpubFromBytes :: ByteString -> Maybe XPub
xpubFromBytes = eitherToMaybe . CC.xpub

-- | Convert an 'XPub' to a raw 'ByteString' (64 bytes).
--
-- @since 1.0.0
xpubToBytes :: XPub -> ByteString
xpubToBytes xpub = xpubPublicKey xpub <> xpubChainCode xpub

-- | Extract the public key from an 'XPub' as a raw 'ByteString' (32 bytes).
--
-- @since 2.0.0
xpubPublicKey :: XPub -> ByteString
xpubPublicKey (CC.XPub pub _cc) = pub

-- | Extract the chain code from an 'XPub' as a raw 'ByteString' (32 bytes).
--
-- @since 2.0.0
xpubChainCode :: XPub -> ByteString
xpubChainCode (CC.XPub _pub (CC.ChainCode cc)) = cc

-- | Construct an 'XPrv' from raw 'ByteString' (96 bytes).
--
-- @since 1.0.0
xprvFromBytes :: ByteString -> Maybe XPrv
xprvFromBytes bytes
    | BS.length bytes /= 96 = Nothing
    | otherwise = do
        let (prv, cc) = BS.splitAt 64 bytes
        pub <- ed25519ScalarMult (BS.take 32 prv)
        eitherToMaybe $ CC.xprv $ prv <> pub <> cc
  where
    ed25519ScalarMult :: ByteString -> Maybe ByteString
    ed25519ScalarMult bs = do
        scalar <- eitherToMaybe $ eitherCryptoError $ Ed25519.scalarDecodeLong bs
        pure $ Ed25519.pointEncode $ Ed25519.toPoint scalar

-- From  < xprv | pub | cc >
-- ↳ To  < xprv |     | cc >
--
-- | Convert an 'XPrv' to a raw 'ByteString' (96 bytes).
--
-- @since 1.0.0
xprvToBytes :: XPrv -> ByteString
xprvToBytes xprv =
    xprvPrivateKey xprv <> xprvChainCode xprv

-- | Extract the private key from an 'XPrv' as a raw 'ByteString' (64 bytes).
--
-- @since 2.0.0
xprvPrivateKey :: XPrv -> ByteString
xprvPrivateKey = BS.take 64 . CC.unXPrv

-- | Extract the chain code from an 'XPrv' as a raw 'ByteString' (32 bytes).
--
-- @since 2.0.0
xprvChainCode :: XPrv -> ByteString
xprvChainCode = BS.drop 96 . CC.unXPrv

-- | Derive the 'XPub' associated with an 'XPrv'.
--
-- @since 1.0.0
toXPub :: HasCallStack => XPrv -> XPub
toXPub = CC.toXPub

-- | Produce a signature of the given 'msg' from an 'XPrv'.
--
-- @since 1.0.0
sign
    :: ByteArrayAccess msg
    => XPrv
    -> msg
    -> XSignature
sign =
    CC.sign (mempty :: ScrubbedBytes)

-- | Verify the 'XSignature' of a 'msg' with the 'XPub' associated with the
-- 'XPrv' used for signing.
--
-- @since 1.0.0
verify
    :: ByteArrayAccess msg
    => XPub
    -> msg
    -> XSignature
    -> Bool
verify =
    CC.verify -- re-exported for the sake of documentation.

-- Derive a child extended private key from an extended private key
--
-- __internal__
deriveXPrv
    :: DerivationScheme
    -> XPrv
    -> Index derivationType depth
    -> XPrv
deriveXPrv ds prv (Index ix) =
    CC.deriveXPrv ds (mempty :: ScrubbedBytes) prv ix

-- Derive a child extended public key from an extended public key
--
-- __internal__
deriveXPub
    :: DerivationScheme
    -> XPub
    -> Index derivationType depth
    -> Maybe XPub
deriveXPub ds pub (Index ix) =
    CC.deriveXPub ds pub ix

-- Generate an XPrv using the legacy method (Byron).
--
-- The seed needs to be at least 32 bytes, otherwise an asynchronous error is thrown.
--
-- __internal__
generate
    :: ByteArrayAccess seed
    => seed
    -> XPrv
generate seed =
    CC.generate seed (mempty :: ScrubbedBytes)

-- Generate an XPrv using the new method (Icarus).
--
-- The seed needs to be at least 16 bytes.
--
-- __internal__
generateNew
    :: (ByteArrayAccess seed, ByteArrayAccess sndFactor)
    => seed
    -> sndFactor
    -> XPrv
generateNew seed sndFactor =
    CC.generateNew seed sndFactor (mempty :: ScrubbedBytes)

-- Hash a credential (pub key or script).
--
-- __internal__
hashCredential :: ByteString -> ByteString
hashCredential =
    BA.convert . hash @_ @Blake2b_224

-- Size, in bytes, of a hash of credential (pub key or script).
--
-- __internal__
credentialHashSize :: Int
credentialHashSize = hashDigestSize Blake2b_224

--
-- Key Derivation
--

-- | Key Depth in the derivation path, according to BIP-0039 / BIP-0044
--
-- @
-- root | purpose' | cointype' | account' | role | address@
-- 0th      1st         2nd        3rd       4th     5th
-- @
--
-- We do not manipulate purpose, cointype and change paths directly, so there
-- are no constructors for these.
--
-- @since 1.0.0
data Depth = RootK | AccountK | PaymentK | DelegationK | ScriptK

-- | A derivation index, with phantom-types to disambiguate derivation type.
--
-- @
-- let accountIx = Index 'Hardened 'AccountK
-- let addressIx = Index 'Soft 'PaymentK
-- @
--
-- @since 1.0.0
newtype Index (derivationType :: DerivationType) (depth :: Depth) = Index
    { getIndex :: Word32 }
    deriving stock (Generic, Show, Eq, Ord)

instance NFData (Index derivationType depth)


instance Bounded (Index 'Hardened depth) where
    minBound = Index 0x80000000
    maxBound = Index maxBound

instance Bounded (Index 'Soft depth) where
    minBound = Index minBound
    maxBound = let (Index ix) = minBound @(Index 'Hardened _) in Index (ix - 1)

instance Bounded (Index 'WholeDomain depth) where
    minBound = Index minBound
    maxBound = Index maxBound

softIndex :: Word32 -> Maybe (Index 'Soft depth)
softIndex = rangedIndex

hardenedIndex :: Word32 -> Maybe (Index 'Hardened depth)
hardenedIndex = rangedIndex

rangedIndex :: forall ix ty depth .
             (ix ~ Index ty depth, Bounded ix)
            => Word32 -> Maybe ix
rangedIndex ix
    | ix >= getIndex (minBound @ix) &&
      ix <= getIndex (maxBound @ix) = Just (Index ix)
    | otherwise = Nothing

wholeDomainIndex :: Word32 -> Index 'WholeDomain depth
wholeDomainIndex ix = Index ix

castWholeDomainIndex :: Index ty depth0 -> Index 'WholeDomain depth1
castWholeDomainIndex (Index ix) = Index ix

instance Buildable (Index derivationType depth) where
    build (Index ix) = fromString (show ix)


-- | Type of derivation that should be used with the given indexes.
--
-- In theory, we should only consider two derivation types: soft and hard.
--
-- However, historically, addresses in Cardano used to be generated across both
-- the soft and the hard domain. We therefore introduce a 'WholeDomain' derivation
-- type that is the exact union of `Hardened` and `Soft`.
--
-- @since 1.0.0
data DerivationType = Hardened | Soft | WholeDomain

-- | An interface for doing hard derivations from the root private key, /Master Key/
--
-- @since 1.0.0
class HardDerivation (key :: Depth -> * -> *) where
    type AccountIndexDerivationType key :: DerivationType
    type AddressIndexDerivationType key :: DerivationType
    type WithRole key :: *

    -- | Derives account private key from the given root private key, using
    -- derivation scheme 2 (see <https://github.com/input-output-hk/cardano-crypto/ cardano-crypto>
    -- package for more details).
    --
    -- @since 1.0.0
    deriveAccountPrivateKey
        :: key 'RootK XPrv
        -> Index (AccountIndexDerivationType key) 'AccountK
        -> key 'AccountK XPrv

    -- | Derives address private key from the given account private key, using
    -- derivation scheme 2 (see <https://github.com/input-output-hk/cardano-crypto/ cardano-crypto>
    -- package for more details).
    --
    -- @since 1.0.0
    deriveAddressPrivateKey
        :: key 'AccountK XPrv
        -> WithRole key
        -> Index (AddressIndexDerivationType key) 'PaymentK
        -> key 'PaymentK XPrv

-- | An interface for doing soft derivations from an account public key
class HardDerivation key => SoftDerivation (key :: Depth -> * -> *) where
    -- | Derives address public key from the given account public key, using
    -- derivation scheme 2 (see <https://github.com/input-output-hk/cardano-crypto/ cardano-crypto>
    -- package for more details).
    --
    -- This is the preferred way of deriving new sequential address public keys.
    --
    -- @since 1.0.0
    deriveAddressPublicKey
        :: key 'AccountK XPub
        -> WithRole key
        -> Index 'Soft 'PaymentK
        -> key 'PaymentK XPub


-- | Abstract interface for constructing a /Master Key/.
--
-- @since 1.0.0
class GenMasterKey (key :: Depth -> * -> *) where
    type SecondFactor key :: *

    -- | Generate a root key from a corresponding mnemonic.
    --
    -- @since 1.0.0
    genMasterKeyFromMnemonic
        :: SomeMnemonic -> SecondFactor key -> key 'RootK XPrv

    -- | Generate a root key from a corresponding root 'XPrv'
    --
    -- @since 1.0.0
    genMasterKeyFromXPrv
        :: XPrv -> key 'RootK XPrv
