{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Address.Style.IcarusSpec
    ( spec
    ) where

import Prelude

import Cardano.Address
    ( PaymentAddress (..), base58 )
import Cardano.Address.Derivation
    ( Depth (..)
    , DerivationType (..)
    , GenMasterKey (..)
    , HardDerivation (..)
    , Index ( getIndex )
    , softIndex
    , hardenedIndex
    , SoftDerivation (..)
    , XPrv
    , toXPub
    )
import Cardano.Address.Style.Icarus
    ( Icarus (..)
    , Role (..)
    , icarusMainnet
    , unsafeGenerateKeyFromHardwareLedger
    )
import Cardano.Mnemonic
    ( ConsistentEntropy
    , EntropySize
    , SomeMnemonic (..)
    , mkMnemonic
    , mkSomeMnemonic
    )
import Control.Monad
    ( forM_ )
import Data.Proxy
    ( Proxy (..) )
import Data.Text
    ( Text )
import Test.Arbitrary
    ( unsafeMkSomeMnemonicFromEntropy )
import Test.Hspec
    ( Spec, describe, it, shouldBe, runIO )
import Test.QuickCheck
    ( Arbitrary (..), Property, arbitraryBoundedEnum, property, (===) )

import qualified Data.Text as T

spec :: Spec
spec = do
    describe "BIP-0044 Derivation Properties" $ do
        it "deriveAccountPrivateKey works for various indexes" $
            property prop_accountKeyDerivation
        it "N(CKDpriv((kpar, cpar), i)) === CKDpub(N(kpar, cpar), i)" $
            property prop_publicChildKeyDerivation

    describe "Golden Tests - Icarus' style addresses" $ do
        let seed0 = unsafeMkSomeMnemonicFromEntropy (Proxy @15)
                "4\175\242L\184\243\191 \169]\171 \207\r\v\233\NUL~&\ETB"
            justSoftIx ix = let Just i = softIndex ix     in i
            justHardIx ix = let Just i = hardenedIndex ix in i

        goldenAddressGeneration $ GoldenAddressGeneration
            seed0 (justHardIx 0x80000000) UTxOExternal (justSoftIx 0x00000000)
            "Ae2tdPwUPEZGQVrA6qKreDzdtYxcWMMrpTFYCpFcuJfhJBEfoeiuW4MtaXZ"

        goldenAddressGeneration $ GoldenAddressGeneration
            seed0 (justHardIx 0x80000000) UTxOExternal (justSoftIx 0x0000000E)
            "Ae2tdPwUPEZDLWQQEBR1UW7HeXJVaqUnuw8DUFu52TDWCJbxbkCyQYyxckP"

        goldenAddressGeneration $ GoldenAddressGeneration
            seed0 (justHardIx 0x8000000E) UTxOInternal (justSoftIx 0x0000002A)
            "Ae2tdPwUPEZFRbyhz3cpfC2CumGzNkFBN2L42rcUc2yjQpEkxDbkPodpMAi"

        let (Right seed1) = mkSomeMnemonic @'[12]
              [ "ghost", "buddy", "neutral", "broccoli", "face", "rack"
              , "relief", "odor", "swallow", "real", "once", "ecology"
              ]

        goldenAddressGeneration $ GoldenAddressGeneration
            seed1 (justHardIx 0x80000000) UTxOExternal (justSoftIx 0x00000000)
            "Ae2tdPwUPEYz6ExfbWubiXPB6daUuhJxikMEb4eXRp5oKZBKZwrbJ2k7EZe"

        goldenAddressGeneration $ GoldenAddressGeneration
            seed1 (justHardIx 0x80000000) UTxOExternal (justSoftIx 0x00000001)
            "Ae2tdPwUPEZCJUCuVgnysar8ZJeyKuhjXU35VNgKMMTcXWmS9zzYycmwKa4"

        goldenAddressGeneration $ GoldenAddressGeneration
            seed1 (justHardIx 0x80000000) UTxOExternal (justSoftIx 0x00000002)
            "Ae2tdPwUPEZFJtMH1m5HvsaQZrmgLcVcyuk5TxYtdRHZFo8yV7yEnnJyqTs"

    describe "Hardware Ledger" $ do
        goldenHardwareLedger @12
            [ "struggle", "section", "scissors", "siren", "garbage", "yellow"
            , "maximum", "finger", "duty", "require", "mule", "earn"
            ]
            [ "Ae2tdPwUPEZ4Gs4s2recjNjQHBKfuBTkeuqbHJJrC6CuyjGyUD44cCTq4sJ"
            , "Ae2tdPwUPEZ8ozZuJWsLVb7aEb5p9ntcja47B9i68GV3y9by1eY5C2y6WUT"
            , "Ae2tdPwUPEZJoUCoyoCxUAKAbn2vFo6nu6B7aTWL1Pv9MRKm8unG9ixLurg"
            , "Ae2tdPwUPEYwFNKLxqF8s31nbaNt5MZisVqsQ5qsiY763HY5wsBN3mSzPRa"
            , "Ae2tdPwUPEZ4ZXzzehKoWWC9QYVqJfEL9x63zjH6wyEJbNRsZ9eccR6nSpv"
            , "Ae2tdPwUPEYyX7ug8zm6K7nLWhgEEBo7Ewf1qALxkvqyHHSC5jMFzH418Q1"
            , "Ae2tdPwUPEZ95eCwDjNQjReRkeLZFv6kBs3vwaKPHJsw2cxXc3HaCD2jzqw"
            , "Ae2tdPwUPEZDHGbQ9sbLZuw3cfhcSzqqdK8Xj3dhAzmWZGeVgJhncu5LR9N"
            , "Ae2tdPwUPEYyDca1eVbeEea6CjihoMAgt6mPiNuC1hEpy5U2qQ1Tzt6E8q8"
            , "Ae2tdPwUPEZHRMjjXMT2icJXp5h2k2j3Ph6dB5iGRashA2QxHLgFZbHzdms"
            ]

        goldenHardwareLedger @18
            [ "vague" , "wrist" , "poet" , "crazy" , "danger" , "dinner"
            , "grace" , "home" , "naive" , "unfold" , "april" , "exile"
            , "relief" , "rifle" , "ranch" , "tone" , "betray" , "wrong"
            ]
            [ "Ae2tdPwUPEZMCGyPAK85FrcserPvzVZZUcbFk5TvDmL9LrUyq2KPYubPcru"
            , "Ae2tdPwUPEZ6drrnNd1KW3UoiU3U1ZK3mxSpQpFAdXzJHuwvDcYB7Wzxkp1"
            , "Ae2tdPwUPEZ7Jaw9qt1q2CjCcds6zpHMyzmPGDh9tBeyQG28AdRGHcaWYx7"
            , "Ae2tdPwUPEZ9SW4qxWkFoozTux5i7F9jVpHQFQUycQuNanSUScyMTYrnQXK"
            , "Ae2tdPwUPEZ6YegpN8XurGfWyKqkNHLgdbHpdohumKt5QpkNVJhw4FCSRdo"
            , "Ae2tdPwUPEZLgrXt3zJeHgFWM2stxRjdm6wWATSoUzJ1CmUxKqgbYQXR8cC"
            , "Ae2tdPwUPEZ6axGCfo5nCLn5hEoRo4yNmQKBzn12B2quPncgQRFP6JBZ2ex"
            , "Ae2tdPwUPEYzdHGmJDL9tEWXfzyshohvzyS3K9wmLc5qMrwRNFPQA611uzB"
            , "Ae2tdPwUPEYxLNQJXcT3XUh54BXn5w53pPe5EHMXo6qo47gpNM9QyJsaXz4"
            , "Ae2tdPwUPEYvq2fnzqs9EWxFF2j87nZzBAZZ7y3qoj5oTce1ZGvsc4potp3"
            ]

        goldenHardwareLedger @24
            [ "recall" , "grace" , "sport" , "punch" , "exhibit" , "mad"
            , "harbor" , "stand" , "obey" , "short" , "width" , "stem"
            , "awkward" , "used" , "stairs" , "wool" , "ugly" , "trap"
            , "season" , "stove" , "worth" , "toward" , "congress" , "jaguar"
            ]
            [ "Ae2tdPwUPEZFvG914wGXtCsb9hCr9aKjJC2ZciLKSNRqAKtjnduH7XtPn78"
            , "Ae2tdPwUPEZ8rVsdBE6EMZpac32MLzciY75MrwrPs8ikjf6MWYFJUHkGaw5"
            , "Ae2tdPwUPEZADQdQy2cbHDwwFRYUcrfreiu82Ngm9Bxdw1pJqJFUnFoQmNL"
            , "Ae2tdPwUPEZ3NULtb3fK6qtJYwJbVnmhDeWzoMbjzPbCsEC9MyB4foBABhz"
            , "Ae2tdPwUPEZ3rGvPCdzCPrVRvzEfpUp8XnZ861nss3XfLun5wA3c3YMA41v"
            , "Ae2tdPwUPEZ575pMY9TBJyPdrwGkq2kr49V9fuqRWpF6wM9JbuZLmxHDo2N"
            , "Ae2tdPwUPEZFaVKwy9bcN81ZPVL8uHRfsrCj7ZZhbm2uqiwLrzsy9Bs1rBN"
            , "Ae2tdPwUPEZ4K16qFm6qVRWTEGpq5TJiyt8ZojmRANTSpPDAWZuH2Ge85uB"
            , "Ae2tdPwUPEZMMYd8JP9F16HJgCsDsPjUoERWoFzZugN4mNjhR9ZnFwPonCs"
            , "Ae2tdPwUPEZ3anXo172NFuumSGjrvbk1pHK9LiF82nGmPKC52NMYR77V2dM"
            ]

{-------------------------------------------------------------------------------
                                 Properties
-------------------------------------------------------------------------------}

-- | Deriving address public key should be equal to deriving address
-- private key and extracting public key from it (works only for non-hardened
-- child keys).
--
-- To compute the public child key of a parent private key:
--  * N(CKDpriv((kpar, cpar), i)) (works always).
--  * CKDpub(N(kpar, cpar), i) (works only for non-hardened child keys).
--
-- Thus:
--
-- N(CKDpriv((kpar, cpar), i)) === CKDpub(N(kpar, cpar), i)
--
-- if (kpar, cpar) is a non-hardened key.
--
-- For details see <https://github.com/bitcoin/bips/blob/master/bip-0032.mediawiki#private-parent-key--public-child-key bip-0039>
prop_publicChildKeyDerivation
    :: SomeMnemonic
    -> Role
    -> Index 'Soft 'PaymentK
    -> Property
prop_publicChildKeyDerivation mw role ix =
    addrXPub1 === addrXPub2
  where
    rootXPrv = genMasterKeyFromMnemonic mw mempty :: Icarus 'RootK XPrv
    accXPrv  = deriveAccountPrivateKey rootXPrv minBound
    -- N(CKDpriv((kpar, cpar), i))
    addrXPub1 = toXPub <$> deriveAddressPrivateKey accXPrv role ix
    -- CKDpub(N(kpar, cpar), i)
    addrXPub2 = deriveAddressPublicKey (toXPub <$> accXPrv) role ix

prop_accountKeyDerivation
    :: SomeMnemonic
    -> Index 'Hardened 'AccountK
    -> Property
prop_accountKeyDerivation mw ix =
    accXPrv `seq` property () -- NOTE Making sure this doesn't throw
  where
    rootXPrv = genMasterKeyFromMnemonic mw mempty :: Icarus 'RootK XPrv
    accXPrv = deriveAccountPrivateKey rootXPrv ix

{-------------------------------------------------------------------------------
                               Golden Tests
-------------------------------------------------------------------------------}

data GoldenAddressGeneration = GoldenAddressGeneration
    { goldSeed :: SomeMnemonic
    , goldAcctIx :: Index 'Hardened 'AccountK
    , goldAcctStyle :: Role
    , goldAddrIx :: Index 'Soft 'PaymentK
    , goldAddr :: Text
    }

-- | Compare addresses obtained from a given derivation path and a root seed to
-- their known equivalent in base58.
goldenAddressGeneration
    :: GoldenAddressGeneration
    -> Spec
goldenAddressGeneration test = it title $ do
    let rootXPrv = genMasterKeyFromMnemonic goldSeed mempty :: Icarus 'RootK XPrv
    let acctXPrv = deriveAccountPrivateKey rootXPrv goldAcctIx
    let addrXPrv = deriveAddressPrivateKey acctXPrv goldAcctStyle goldAddrIx
    base58 (paymentAddress icarusMainnet $ toXPub <$> addrXPrv)
        `shouldBe` goldAddr
  where
    GoldenAddressGeneration
        { goldSeed
        , goldAddr
        , goldAcctIx
        , goldAddrIx
        , goldAcctStyle
        } = test

    title = unwords
        [ fmtPath goldAcctIx goldAcctStyle goldAddrIx
        , "-->"
        , T.unpack goldAddr
        ]

    -- e.g. m/.../0'/0/0
    fmtPath p3 p4 p5 = mconcat
        [ "m/.../"
        , show (getIndex p3 - getIndex (minBound @(Index 'Hardened _)))
        , "'/"
        , show (fromEnum p4)
        , "/"
        , show (getIndex p5)
        ]

goldenHardwareLedger
    :: forall mw ent csz.
        ( ConsistentEntropy ent mw csz
        , EntropySize mw ~ ent
        )
    => [Text]
        -- ^ 24-word mnemonic
    -> [Text]
        -- ^ Some addresses, starting at index 0
    -> Spec
goldenHardwareLedger sentence addrs =
    it title $ do
        let Right mnemonic = SomeMnemonic <$> mkMnemonic @mw sentence
        let rootXPrv = unsafeGenerateKeyFromHardwareLedger mnemonic
        let acctXPrv = deriveAccountPrivateKey rootXPrv minBound
        let deriveAddr = deriveAddressPrivateKey acctXPrv UTxOExternal

        forM_ (zip [0..] addrs) $ \(ix, addr) -> do
            let Just softIx = softIndex ix
                addrXPrv = deriveAddr softIx
            base58 (paymentAddress icarusMainnet $ toXPub <$> addrXPrv)
                `shouldBe` addr
  where
    title = T.unpack
        $ T.unwords
        $ take 3 sentence ++ [ "..." ] ++ drop (length sentence - 3) sentence

instance Arbitrary Role where
    shrink _ = []
    arbitrary = arbitraryBoundedEnum
