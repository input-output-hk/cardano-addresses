{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}

module Test.Arbitrary
    ( unsafeMkMnemonic
    , unsafeMkSomeMnemonicFromEntropy
    , unsafeFromHex
    ) where

import Prelude

import Cardano.Address
    ( AddressDiscrimination (..), ChainPointer (..), NetworkTag (..) )
import Cardano.Address.Derivation
    ( Depth (..)
    , DerivationType (..)
    , GenMasterKey (..)
    , HardDerivation (..)
    , Index (getIndex)
    , softIndex
    , hardenedIndex
    , wholeDomainIndex
    , XPrv
    , XPub
    , generate
    , generateNew
    , toXPub
    , xprvToBytes
    )
import Cardano.Address.Style.Byron
    ( Byron, byronMainnet, byronStaging, byronTestnet )
import Cardano.Address.Style.Icarus
    ( Icarus, icarusMainnet, icarusStaging, icarusTestnet )
import Cardano.Address.Style.Shelley
    ( Shelley )
import Cardano.Mnemonic
    ( ConsistentEntropy
    , Entropy
    , EntropySize
    , Mnemonic
    , MnemonicException (..)
    , MnemonicWords
    , SomeMnemonic (..)
    , entropyToMnemonic
    , mkEntropy
    , mkMnemonic
    )
import Crypto.Encoding.BIP39
    ( ValidChecksumSize, ValidEntropySize, ValidMnemonicSentence )
import Data.ByteArray.Encoding
    ( Base (..), convertFromBase )
import Data.ByteString
    ( ByteString )
import Data.Function
    ( on )
import Data.Maybe
    ( fromMaybe )
import Data.Proxy
    ( Proxy (..) )
import Data.Text
    ( Text )
import Data.Word
    ( Word32, Word64 )
import GHC.Stack
    ( HasCallStack )
import GHC.TypeLits
    ( natVal )
import Numeric.Natural
    ( Natural )
import Test.QuickCheck
    ( Arbitrary (..), Gen, arbitraryBoundedEnum, choose, oneof, vector )

import qualified Cardano.Address.Style.Shelley as Shelley
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8

--
-- Arbitrary Instances
--

-- | The initial seed has to be vector or length multiple of 4 bytes and shorter
-- than 64 bytes. Note that this is good for testing or examples, but probably
-- not for generating truly random Mnemonic words.
--
-- See 'Crypto.Random.Entropy (getEntropy)'
instance
    ( ValidEntropySize n
    , ValidChecksumSize n csz
    ) => Arbitrary (Entropy n) where
    arbitrary =
        let
            size = fromIntegral $ natVal @n Proxy
            entropy =
                mkEntropy  @n . BA.convert . B8.pack <$> vector (size `quot` 8)
        in
            either (error . show . UnexpectedEntropyError) id <$> entropy

-- | Same remark from 'Arbitrary Entropy' applies here.
instance
    ( n ~ EntropySize mw
    , mw ~ MnemonicWords n
    , ValidChecksumSize n csz
    , ValidEntropySize n
    , ValidMnemonicSentence mw
    , Arbitrary (Entropy n)
    ) => Arbitrary (Mnemonic mw) where
    arbitrary =
        entropyToMnemonic <$> arbitrary @(Entropy n)

instance Arbitrary (Index 'Soft 'PaymentK) where
    shrink _ = []
    arbitrary = let err = error "Arbitrary (Index 'Soft 'PaymentK)"
                in  arbitraryIndex (fromMaybe err . softIndex)

instance Arbitrary (Index 'Hardened 'AccountK) where
    shrink _ = []
    arbitrary = let err = error "Arbitrary (Index 'Hardened 'AccountK)"
                in  arbitraryIndex (fromMaybe err . hardenedIndex)

instance Arbitrary (Index 'Hardened 'PaymentK) where
    shrink _ = []
    arbitrary = let err = error "Arbitrary (Index 'Hardened 'PaymentK)"
                in  arbitraryIndex (fromMaybe err . hardenedIndex)

instance Arbitrary (Index 'WholeDomain 'PaymentK) where
    shrink _ = []
    arbitrary = arbitraryIndex wholeDomainIndex

instance Arbitrary (Index 'WholeDomain 'AccountK) where
    shrink _ = []
    arbitrary = arbitraryIndex wholeDomainIndex

arbitraryIndex :: forall ix ty depth . (Bounded ix, ix ~ Index ty depth)
               => (Word32 -> ix) -> Gen ix
arbitraryIndex f = f <$> choose ( getIndex (minBound @ix)
                                , getIndex (maxBound @ix))

instance Arbitrary SomeMnemonic where
    arbitrary = SomeMnemonic <$> genMnemonic @12

instance Arbitrary XPrv where
    arbitrary = oneof
        [ flip generateNew (mempty :: ByteString) . BS.pack <$> vector 16
        , generate . BS.pack <$> vector 32
        ]

instance Arbitrary XPub where
    arbitrary =
        toXPub <$> arbitrary

instance Arbitrary (Byron 'PaymentK XPub) where
    shrink _ = []
    arbitrary = do
        mw <- SomeMnemonic <$> genMnemonic @12
        rootK <- genMasterKeyFromMnemonic mw   <$> arbitrary
        acctK <- deriveAccountPrivateKey rootK <$> arbitrary
        addrK <- deriveAddressPrivateKey acctK () <$> arbitrary
        pure $ toXPub <$> addrK

instance Arbitrary (Icarus 'PaymentK XPub) where
    shrink _ = []
    arbitrary = do
        mw <- SomeMnemonic <$> genMnemonic @15
        bytes <- BA.convert . BS.pack <$> (choose (0, 32) >>= vector)
        let rootK = genMasterKeyFromMnemonic mw bytes
        acctK <- deriveAccountPrivateKey rootK <$> arbitrary
        let roleGen =  arbitraryBoundedEnum
        addrK <- deriveAddressPrivateKey acctK <$> roleGen <*> arbitrary
        pure $ toXPub <$> addrK

instance Arbitrary (Shelley 'PaymentK XPub) where
    shrink _ = []
    arbitrary = do
        mw <- SomeMnemonic <$> genMnemonic @15
        bytes <- BA.convert . BS.pack <$> (choose (0, 32) >>= vector)
        let rootK = genMasterKeyFromMnemonic mw bytes
        acctK <- deriveAccountPrivateKey rootK <$> arbitrary
        let roleGen = arbitraryBoundedEnum
        addrK <- deriveAddressPrivateKey acctK <$> roleGen <*> arbitrary
        pure $ toXPub <$> addrK

instance Arbitrary (Shelley 'DelegationK XPub) where
    shrink _ = []
    arbitrary = do
        mw <- SomeMnemonic <$> genMnemonic @15
        bytes <- BA.convert . BS.pack <$> (choose (0, 32) >>= vector)
        let rootK = genMasterKeyFromMnemonic mw bytes
        acctK <- deriveAccountPrivateKey rootK <$> arbitrary
        let delegationK = Shelley.deriveDelegationPrivateKey acctK
        pure $ toXPub <$> delegationK

instance {-# OVERLAPS #-} Arbitrary (AddressDiscrimination, NetworkTag) where
    arbitrary = oneof
        -- NOTE using explicit smart-constructor as a quick-win for the coverage :)
        [ (RequiresNoTag,) <$> arbitrary
        , (RequiresNetworkTag,) <$> arbitrary
        , pure byronMainnet
        , pure byronStaging
        , pure byronTestnet
        , pure icarusMainnet
        , pure icarusStaging
        , pure icarusTestnet
        ]

instance Arbitrary NetworkTag where
    shrink (NetworkTag tag) = NetworkTag <$> shrink tag
    arbitrary = NetworkTag <$> choose (0, 15)

instance Arbitrary Natural where
    arbitrary =
        fromIntegral <$> choose (1 :: Word64, 10000000000)

instance Arbitrary ChainPointer where
    arbitrary = do
        slot <- arbitrary
        ix1 <- fromIntegral <$> choose (1 :: Word64, 1000000)
        ix2 <- fromIntegral <$> choose (1 :: Word64, 1000000)
        pure $ ChainPointer slot ix1 ix2

--
-- Extra Instances
--

-- Necessary unsound Show instance for QuickCheck failure reporting
instance Show XPrv where
    show = show . xprvToBytes

-- Necessary unsound Eq instance for QuickCheck properties
instance Eq XPrv where
    (==) = (==) `on` xprvToBytes

--
-- Useful functions
--

-- | Generates an arbitrary mnemonic of a size according to the type parameter.
--
-- E.g:
-- >>> arbitrary = SomeMnemonic <$> genMnemonic @12
genMnemonic
    :: forall mw ent csz.
     ( ConsistentEntropy ent mw csz
     , EntropySize mw ~ ent
     )
    => Gen (Mnemonic mw)
genMnemonic = do
        let n = fromIntegral (natVal $ Proxy @(EntropySize mw)) `div` 8
        bytes <- BS.pack <$> vector n
        let ent = unsafeMkEntropy @(EntropySize mw) bytes
        return $ entropyToMnemonic ent

unsafeMkEntropy
    :: forall ent csz.
        ( HasCallStack
        , ValidEntropySize ent
        , ValidChecksumSize ent csz
        )
    => ByteString
    -> Entropy ent
unsafeMkEntropy = either (error . show) id . mkEntropy . BA.convert

-- | Build 'Mnemonic' from literals
unsafeMkMnemonic
    :: forall mw n csz
    .  (ConsistentEntropy n mw csz, EntropySize mw ~ n, HasCallStack)
    => [Text]
    -> Mnemonic mw
unsafeMkMnemonic m =
    case mkMnemonic m of
        Left e -> error $ "unsafeMnemonic: " <> show e
        Right a -> a

unsafeMkSomeMnemonicFromEntropy
    :: forall mw ent csz.
        ( HasCallStack
        , ValidEntropySize ent
        , ValidChecksumSize ent csz
        , ValidMnemonicSentence mw
        , ent ~ EntropySize mw
        , mw ~ MnemonicWords ent
        )
    => Proxy mw
    -> ByteString
    -> SomeMnemonic
unsafeMkSomeMnemonicFromEntropy _ = SomeMnemonic
    . entropyToMnemonic
    . unsafeMkEntropy @ent

-- | Decode an hex-encoded 'ByteString' into raw bytes, or fail.
unsafeFromHex :: HasCallStack => ByteString -> ByteString
unsafeFromHex =
    either (error . show) id . convertFromBase @ByteString @ByteString Base16
