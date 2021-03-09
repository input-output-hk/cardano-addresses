{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Address.Style.ShelleySpec
    ( spec
    ) where

import Prelude

import Cardano.Address
    ( Address
    , ChainPointer (..)
    , HasNetworkDiscriminant (..)
    , HasNetworkDiscriminant (NetworkDiscriminant)
    , bech32
    , bech32With
    , fromBech32
    , unsafeMkAddress
    )
import Cardano.Address.Derivation
    ( Depth (..)
    , DerivationType (..)
    , GenMasterKey (..)
    , HardDerivation (..)
    , Index
    , hardenedIndex
    , softIndex
    , SoftDerivation (..)
    , XPrv
    , XPub
    , toXPub
    , xprvToBytes
    , xpubFromBytes
    , xpubToBytes
    )
import Cardano.Address.Style.Shelley
    ( Credential (..)
    , Role (..)
    , Shelley (..)
    , delegationAddress
    , deriveDelegationPrivateKey
    , deriveMultisigForDelegationPrivateKey
    , deriveMultisigForDelegationPublicKey
    , deriveMultisigForPaymentPrivateKey
    , deriveMultisigForPaymentPublicKey
    , liftXPub
    , mkNetworkDiscriminant
    , paymentAddress
    , pointerAddress
    )
import Cardano.Mnemonic
    ( SomeMnemonic, mkSomeMnemonic )
import Codec.Binary.Encoding
    ( AbstractEncoding (..), encode, fromBase16 )
import Data.ByteArray
    ( ByteArrayAccess, ScrubbedBytes )
import Data.ByteString
    ( ByteString )
import Data.Either
    ( rights )
import Data.Function
    ( (&) )
import Data.Text
    ( Text )
import Test.Arbitrary
    ()
import Test.Hspec
    ( Spec, SpecWith, describe, it, shouldBe )
import Test.Hspec.QuickCheck
    ( prop )
import Test.QuickCheck
    ( Arbitrary (..)
    , Property
    , arbitraryBoundedEnum
    , choose
    , counterexample
    , expectFailure
    , label
    , property
    , vector
    , (===)
    , (==>)
    )

import qualified Cardano.Address.Style.Shelley as Shelley
import qualified Codec.Binary.Bech32 as Bech32
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

spec :: Spec
spec = do
    describe "BIP-0044 Derivation Properties" $ do
        it "deriveAccountPrivateKey works for various indexes" $
            property prop_accountKeyDerivation
        it "N(CKDpriv((kpar, cpar), i)) === CKDpub(N(kpar, cpar), i) for any key" $
            property prop_publicChildKeyDerivation
        it "N(CKDpriv((kpar, cpar), i)) === CKDpub(N(kpar, cpar), i) for multisig for payment credential" $
            property prop_publicMultisigForPaymentDerivation
        it "N(CKDpriv((kpar, cpar), i)) === CKDpub(N(kpar, cpar), i) for multisig for delegation credential" $
            property prop_publicMultisigForDelegationDerivation

    describe "Bounded / Enum relationship" $ do
        it "Calling toEnum for invalid value gives a runtime err (Role)"
            (property prop_toEnumRole)

    describe "Enum Roundtrip" $ do
        it "Role" (property prop_roundtripEnumRole)

    describe "Proper pointer addresses construction" $ do
        it "Using different numbers in DelegationPointerAddress does not fail"
            (property prop_pointerAddressConstruction)

    describe "Text Encoding Roundtrips" $ do
        prop "bech32 . fromBech32 - Shelley - payment address" $
            prop_roundtripTextEncoding bech32 fromBech32

        prop "bech32 . fromBech32 - Shelley - delegation address" $
            prop_roundtripTextEncodingDelegation bech32 fromBech32

        prop "bech32 . fromBech32 - Shelley - pointer address" $
            prop_roundtripTextEncodingPointer bech32 fromBech32

    describe "Golden tests" $ do
        goldenTestPointerAddress GoldenTestPointerAddress
            {  verKey = "1a2a3a4a5a6a7a8a"
            ,  stakePtr = ChainPointer 128 2 3
            ,  netTag = 0
            ,  expectedAddr =
                    "408a4d111f71a79169c50bcbc27e1e20b6e13e87ff8f33edc3cab419d4\
                    \81000203"
            }
        goldenTestPointerAddress GoldenTestPointerAddress
            {  verKey = "1a2a3a4a5a6a7a8a"
            ,  stakePtr = ChainPointer 128 2 3
            ,  netTag = 1
            ,  expectedAddr =
                    "418a4d111f71a79169c50bcbc27e1e20b6e13e87ff8f33edc3cab419d4\
                    \81000203"
            }
        goldenTestEnterpriseAddress GoldenTestEnterpriseAddress
            {  verKey = "1a2a3a4a5a6a7a8a"
            ,  netTag = 0
            ,  expectedAddr =
                    "608a4d111f71a79169c50bcbc27e1e20b6e13e87ff8f33edc3cab419d4"
            }
        goldenTestEnterpriseAddress GoldenTestEnterpriseAddress
            {  verKey = "1a2a3a4a5a6a7a8a"
            ,  netTag = 1
            ,  expectedAddr =
                    "618a4d111f71a79169c50bcbc27e1e20b6e13e87ff8f33edc3cab419d4"
            }
        goldenTestBaseAddress GoldenTestBaseAddress
            {  verKeyPayment = "1a2a3a4a5a6a7a8a"
            ,  verKeyStake = "1c2c3c4c5c6c7c8c"
            ,  netTag = 0
            ,  expectedAddr =
                    "008a4d111f71a79169c50bcbc27e1e20b6e13e87ff8f33edc3cab419d4\
                    \08b2d658668c2e341ee5bda4477b63c5aca7ec7ae4e3d196163556a4"
            }
        goldenTestBaseAddress GoldenTestBaseAddress
            {  verKeyPayment = "1a2a3a4a5a6a7a8a"
            ,  verKeyStake = "1c2c3c4c5c6c7c8c"
            ,  netTag = 1
            ,  expectedAddr =
                    "018a4d111f71a79169c50bcbc27e1e20b6e13e87ff8f33edc3cab419d4\
                    \08b2d658668c2e341ee5bda4477b63c5aca7ec7ae4e3d196163556a4"
            }

    describe "Test vectors" $ do
        testVectors TestVector
            { rootXPrv =
                    "root_xprv1nq49elkxhet9d03y0uqxp5fa802jgqauef3878ve3r9xvkg8\
                    \c4qqnq98raswxtrwlfw7txa7m49d6x84092l0lsv5s603dkakj8vwgsz23\
                    \drjsarjmxdev8mnrllmxykxh7exsrtqn64d6j5qdqp9deqpqx70t25"
            , accXPrv0 =
                    "acct_xprv1xqwvl9u488wkkywa9pfr8vncxf6sk6reea878ytt8pgrses8\
                    \c4q9wl66mqw6gtcz7jrw2x5nh0c9vxl4u4yk35qmuqs62hwmfw8g59nqqz\
                    \sw5cdy26sfmu8y8q9g36w0ukrh62g5x9x2r3p7g6x8z064nulavjdy"
            , accXPrv1 =
                    "acct_xprv13pxyc73zy9eagvj84jdq5fmqcvsnpln64gmldtm69k4cses8\
                    \c4qzzcx7qvdtjjl48r0xr95xrdffc9rn5ahus7sgl5f36hvd5ha9j0z8gx\
                    \ffjqs8qsuayeaadwevup6jd8vfrn874c7je3400lwlhxy7nvv6dmmy"
            , addrXPrv0 =
                    "addr_xprv1dzrs9tvy632mtzvjgrvrkdr0j96j4dwaej06lytjahvzquq8\
                    \c4qzqws3zp2uc8rmrmhzrhukjml9vrfqxwhpjs78yfh6nyv9cpk9n87e4r\
                    \smperder5gf9wje76zuq39w7vaefaehkvd0vyt0802sryq053jdle4"
            , addrXPub0 =
                    "addr_xpub1lrh9ds9kxhhnhqmpqfsj3wrf48mcs0m8r95c9ggdwe3cltc7\
                    \xvran28pkrjxmj8gsj2a9na59cpz2auemjnmn0vc67cgk7w74qxgqlg0ydscd"
            , addrXPrv1 =
                    "addr_xprv16qyf2nkx9t7qunjqza480djyfednzjrsa0407qyujexnzag8\
                    \c4qz48ctgnzfgw8m8mdawu9wg2d7ezt27zfexp6ffzpz36spycusdteq89\
                    \8ucwnylsl36xhdjum7xa8mtw4ga358qkv80dppmwyjmgy7g5ke0uyw"
            , addrXPub1 =
                    "addr_xpub10thqpnm6uuksmr04tdwz6vlgm6wrnccd5mlglzjkr39l6e2p\
                    \qa4jqw20esaxflplr5dwm9ehud60kka23mrgwpvcw76zrkuf9ksfu3gvlxlc9"
            , addrXPrv1442 =
                    "addr_xprv1pqtsmncr23nfc86v4pallgj6yv2pfttalmjpa2unme08gug8\
                    \c4qgz9jme868mkxrjen2hfwcu6swa6s7z608gy2y62l2082698fkvvkvld\
                    \3hnqave3c98llkdelz0qa9frxyjxggfazye0g2ptp5k7vpgcuqa764"
            , addrXPub1442 =
                    "addr_xpub1xq255jrlf0cxwu9kqe9sdyljhvn972dez5f2fg2zfpc4dt0f\
                    \gk5ue7mr0xp6enrs20llvmn7y7p62jxvfyvssn6yfj7s5zkrfducz3sc6gjjx"
            , paymentAddr0  =
                    [ "addr1vqtnpvdhqrtpd4g424fcaq7k0ufuzyadt7djygf8qdyzevc4r0hla"
                    , "addr1vvtnpvdhqrtpd4g424fcaq7k0ufuzyadt7djygf8qdyzevc4ukdl5"
                    , "addr1vctnpvdhqrtpd4g424fcaq7k0ufuzyadt7djygf8qdyzevc4552l0"
                    ]
            , paymentAddr1  =
                    [ "addr1vzmetq35wm8yeqhjfx4umn75vs28ufpqq52a9f7ng0an2wst60898"
                    , "addr1vwmetq35wm8yeqhjfx4umn75vs28ufpqq52a9f7ng0an2wst9ka9w"
                    , "addr1v6metq35wm8yeqhjfx4umn75vs28ufpqq52a9f7ng0an2wstd5694"
                    ]
            , paymentAddr1442  =
                    [ "addr1vrfx5w509r5mmqle3kxhue2zep2vknjvwkrwsr80asqq3ugy06ne0"
                    , "addr1v0fx5w509r5mmqle3kxhue2zep2vknjvwkrwsr80asqq3ugysrfex"
                    , "addr1vmfx5w509r5mmqle3kxhue2zep2vknjvwkrwsr80asqq3ugycpwea"
                    ]
            , delegationAddr0Stake0 =
                    [ "addr1qqtnpvdhqrtpd4g424fcaq7k0ufuzyadt7djygf8qdyzeva20vj\
                      \rx7u3sk3qnfndth3c8eha7d8ar7xyf068xpyudqwsk63f96"
                    , "addr1qvtnpvdhqrtpd4g424fcaq7k0ufuzyadt7djygf8qdyzeva20vj\
                      \rx7u3sk3qnfndth3c8eha7d8ar7xyf068xpyudqwsvf2q65"
                    , "addr1qctnpvdhqrtpd4g424fcaq7k0ufuzyadt7djygf8qdyzeva20vj\
                      \rx7u3sk3qnfndth3c8eha7d8ar7xyf068xpyudqwst4wmjx"]
            , delegationAddr1Stake0 =
                    [ "addr1qzmetq35wm8yeqhjfx4umn75vs28ufpqq52a9f7ng0an2w420vj\
                      \rx7u3sk3qnfndth3c8eha7d8ar7xyf068xpyudqwsy955sd"
                    , "addr1qwmetq35wm8yeqhjfx4umn75vs28ufpqq52a9f7ng0an2w420vj\
                      \rx7u3sk3qnfndth3c8eha7d8ar7xyf068xpyudqws7k0a0r"
                    , "addr1q6metq35wm8yeqhjfx4umn75vs28ufpqq52a9f7ng0an2w420vj\
                      \rx7u3sk3qnfndth3c8eha7d8ar7xyf068xpyudqwse2tx83"
                    ]
            , delegationAddr1442Stake0 =
                    [ "addr1qrfx5w509r5mmqle3kxhue2zep2vknjvwkrwsr80asqq3ud20vj\
                      \rx7u3sk3qnfndth3c8eha7d8ar7xyf068xpyudqws6k6w7c"
                    , "addr1q0fx5w509r5mmqle3kxhue2zep2vknjvwkrwsr80asqq3ud20vj\
                      \rx7u3sk3qnfndth3c8eha7d8ar7xyf068xpyudqwsq9p8pk"
                    , "addr1qmfx5w509r5mmqle3kxhue2zep2vknjvwkrwsr80asqq3ud20vj\
                      \rx7u3sk3qnfndth3c8eha7d8ar7xyf068xpyudqws8e9ufy"
                    ]
            , delegationAddr0Stake1 =
                    [ "addr1qqtnpvdhqrtpd4g424fcaq7k0ufuzyadt7djygf8qdyzevudycp\
                      \flwuhen8hqvhm59wg8dakkdzg40ua0ukpmlqu803q70sx8s"
                    , "addr1qvtnpvdhqrtpd4g424fcaq7k0ufuzyadt7djygf8qdyzevudycp\
                      \flwuhen8hqvhm59wg8dakkdzg40ua0ukpmlqu803qyut0c7"
                    , "addr1qctnpvdhqrtpd4g424fcaq7k0ufuzyadt7djygf8qdyzevudycp\
                      \flwuhen8hqvhm59wg8dakkdzg40ua0ukpmlqu803qrq05sv"
                    ]
            , delegationAddr1Stake1 =
                    [ "addr1qzmetq35wm8yeqhjfx4umn75vs28ufpqq52a9f7ng0an2w5dycp\
                      \flwuhen8hqvhm59wg8dakkdzg40ua0ukpmlqu803qvs4mj8"
                    , "addr1qwmetq35wm8yeqhjfx4umn75vs28ufpqq52a9f7ng0an2w5dycp\
                      \flwuhen8hqvhm59wg8dakkdzg40ua0ukpmlqu803qkrwjdf"
                    , "addr1q6metq35wm8yeqhjfx4umn75vs28ufpqq52a9f7ng0an2w5dycp\
                      \flwuhen8hqvhm59wg8dakkdzg40ua0ukpmlqu803q3l2f9m"
                    ]
            , delegationAddr1442Stake1 =
                    [ "addr1qrfx5w509r5mmqle3kxhue2zep2vknjvwkrwsr80asqq3uvdycp\
                      \flwuhen8hqvhm59wg8dakkdzg40ua0ukpmlqu803qjrmpuj"
                    , "addr1q0fx5w509r5mmqle3kxhue2zep2vknjvwkrwsr80asqq3uvdycp\
                      \flwuhen8hqvhm59wg8dakkdzg40ua0ukpmlqu803qgsqgru"
                    , "addr1qmfx5w509r5mmqle3kxhue2zep2vknjvwkrwsr80asqq3uvdycp\
                      \flwuhen8hqvhm59wg8dakkdzg40ua0ukpmlqu803q0vyntw"
                    ]
            , pointerAddr0Slot1 =
                    [ "addr1gqtnpvdhqrtpd4g424fcaq7k0ufuzyadt7djygf8qdyzevcpqgpsz4dfdc"
                    , "addr1gvtnpvdhqrtpd4g424fcaq7k0ufuzyadt7djygf8qdyzevcpqgpsgl562s"
                    , "addr1gctnpvdhqrtpd4g424fcaq7k0ufuzyadt7djygf8qdyzevcpqgpskpkxrg"
                    ]
            , pointerAddr0Slot2 =
                    [ "addr1gqtnpvdhqrtpd4g424fcaq7k0ufuzyadt7djygf8qdyzevuph3wczvf2dwyx5u"
                    , "addr1gvtnpvdhqrtpd4g424fcaq7k0ufuzyadt7djygf8qdyzevuph3wczvf2p4222m"
                    , "addr1gctnpvdhqrtpd4g424fcaq7k0ufuzyadt7djygf8qdyzevuph3wczvf243c7pj"
                    ]
            , mnemonic = [ "test", "child", "burst", "immense", "armed", "parrot"
                         , "company", "walk", "dog" ]
            }
        testVectors TestVector
            { rootXPrv =
                    "root_xprv1vzrzr76vqyqlavclduhawqvtae2pq8lk0424q7t8rzfjyhhp\
                    \530zxv2fwq5a3pd4vdzqtu6s2zxdjhww8xg4qwcs7y5dqne5k7mz27p6rc\
                    \aath83rl20nz0v9nwdaga9fkufjuucza8vmny8qpkzwstk5qwutx5p"
            , accXPrv0 =
                    "acct_xprv12phl7y4uv58mne08me5szrwly2gn6jasqmkvcrndq762q68p\
                    \5302z24gdf365klhll2a5f357k7nc4kpaq7j6agr5m22jq4jwlfv6l505h\
                    \7dg64an4rdfk9f028nge0zcn508jw6m8lkdq36zc0v4h9xqs762yl0"
            , accXPrv1 =
                    "acct_xprv1ppjrs9d7yh6qzmzng34nyh5ysx4uzewz09ndyjudgep0x6lp\
                    \530vzh0gmwrt53p3h55l8ect5cw95ejpgj667eawewrcmrf2ajda4mzxlz\
                    \pf2w63cwazlp9226pr4m6s35lawkmrqv8gf8ycl0ernjf69vpufpq7"
            , addrXPrv0 =
                    "addr_xprv1hqf6v2lvhfn5mr3fe6g8ac6n8a3z6s0p24mg6kre8jadxulp\
                    \530y07wjp2ml0zcz8gk0xc7zy96qp2xxtr0arjq9038k9dhkw3k3cswawh\
                    \s4fkjp00kwc4wd6fynyaz5zw8ssggs9974apatyhs4ltg4puskm3kd"
            , addrXPub0 =
                    "addr_xpub1w0l2sr2zgfm26ztc6nl9xy8ghsk5sh6ldwemlpmp9xylzy4d\
                    \tf7a6a0p2ndyz7lva32um5jfxf69gyu0pqs3q2tat6r6kf0pt7k32rcmm5vlw"
            , addrXPrv1 =
                    "addr_xprv1up30yhwmkujfkjd2rvy99z7qg7mdpqd8n7vehsyvfq20zulp\
                    \530weme6mjk3yuqkynms9tr23wqczk94npvnp4r8l2vrq36l8vudk8lh4v\
                    \fx72yymwg9n7sfe2pmu6ut6qjsgf4wkcser0wesc290w9ujyg4af0a"
            , addrXPub1 =
                    "addr_xpub17cn2hzr7kh6qk5pyv0x09mz6wvgkwmhfuh24cjfqtx3kds95\
                    \6jsl02cjdu5gfkust8aqnj5rhe4ch5p9qsn2ad3pjx7anps527uteyg7w980n"
            , addrXPrv1442 =
                    "addr_xprv14qtc6t6wfzk4jvdp0k8d9jp0xtlmhkh8el98nfnqwr5kqulp\
                    \530vmhqefsml4rsntysxu057jecdq0l3gmzs6c8nv5mvjn5aesmklzqepr\
                    \3e4n4ca0p23tm57dwkw4ws46cz2mmdx3jnxtxy8zqyfekftvu7wdvm"
            , addrXPub1442 =
                    "addr_xpub1l7vftt4ayg29thue83m2crav65wqzak2v9v7lryjnnphq88r\
                    \srr3jz8rnt8t367z4zhhfu6ava2apt4sy4hk6dr9xvkvgwyqgnnvjkcrvvkp8"
            , paymentAddr0  =
                    [ "addr1vz2fxv2umyhttkxyxp8x0dlpdt3k6cwng5pxj3jhsydzers6g8jlq"
                    , "addr1vw2fxv2umyhttkxyxp8x0dlpdt3k6cwng5pxj3jhsydzers6h7glf"
                    , "addr1v62fxv2umyhttkxyxp8x0dlpdt3k6cwng5pxj3jhsydzers6lu0lj"
                    ]
            , paymentAddr1  =
                    [ "addr1vz7svwszky8gcmhrfza7a89z9u0dfzd3l7h23sqlc5yml7clpju0e"
                    , "addr1vw7svwszky8gcmhrfza7a89z9u0dfzd3l7h23sqlc5yml7cl7tx0s"
                    , "addr1v67svwszky8gcmhrfza7a89z9u0dfzd3l7h23sqlc5yml7clkfp0t"
                    ]
            , paymentAddr1442  =
                    [ "addr1vrlrt7d6ssjdr6kjykk5xtvcwdhysw3455ukq58mlakmwtqtdphx8"
                    , "addr1v0lrt7d6ssjdr6kjykk5xtvcwdhysw3455ukq58mlakmwtqtjcdxw"
                    , "addr1vmlrt7d6ssjdr6kjykk5xtvcwdhysw3455ukq58mlakmwtqt662x4"
                    ]
            , delegationAddr0Stake0 =
                    [ "addr1qz2fxv2umyhttkxyxp8x0dlpdt3k6cwng5pxj3jhsydzer3jcu5\
                      \d8ps7zex2k2xt3uqxgjqnnj83ws8lhrn648jjxtwqcyl47r"
                    , "addr1qw2fxv2umyhttkxyxp8x0dlpdt3k6cwng5pxj3jhsydzer3jcu5\
                      \d8ps7zex2k2xt3uqxgjqnnj83ws8lhrn648jjxtwqzhyupd"
                    , "addr1q62fxv2umyhttkxyxp8x0dlpdt3k6cwng5pxj3jhsydzer3jcu5\
                      \d8ps7zex2k2xt3uqxgjqnnj83ws8lhrn648jjxtwq9tq8fl"
                    ]
            , delegationAddr1Stake0 =
                    [ "addr1qz7svwszky8gcmhrfza7a89z9u0dfzd3l7h23sqlc5yml7ejcu5\
                      \d8ps7zex2k2xt3uqxgjqnnj83ws8lhrn648jjxtwq2qhtad"
                    , "addr1qw7svwszky8gcmhrfza7a89z9u0dfzd3l7h23sqlc5yml7ejcu5\
                      \d8ps7zex2k2xt3uqxgjqnnj83ws8lhrn648jjxtwqsnvzzr"
                    , "addr1q67svwszky8gcmhrfza7a89z9u0dfzd3l7h23sqlc5yml7ejcu5\
                      \d8ps7zex2k2xt3uqxgjqnnj83ws8lhrn648jjxtwqh0ge23"
                    ]
            , delegationAddr1442Stake0 =
                    [ "addr1qrlrt7d6ssjdr6kjykk5xtvcwdhysw3455ukq58mlakmwtpjcu5\
                      \d8ps7zex2k2xt3uqxgjqnnj83ws8lhrn648jjxtwq9z8tqw"
                    , "addr1q0lrt7d6ssjdr6kjykk5xtvcwdhysw3455ukq58mlakmwtpjcu5\
                      \d8ps7zex2k2xt3uqxgjqnnj83ws8lhrn648jjxtwql3uzlq"
                    , "addr1qmlrt7d6ssjdr6kjykk5xtvcwdhysw3455ukq58mlakmwtpjcu5\
                      \d8ps7zex2k2xt3uqxgjqnnj83ws8lhrn648jjxtwqcdcehj"
                    ]
            , delegationAddr0Stake1 =
                    [ "addr1qz2fxv2umyhttkxyxp8x0dlpdt3k6cwng5pxj3jhsydzer3n0d3\
                      \vllmyqwsx5wktcd8cc3sq835lu7drv2xwl2wywfgsg8a6rm"
                    , "addr1qw2fxv2umyhttkxyxp8x0dlpdt3k6cwng5pxj3jhsydzer3n0d3\
                      \vllmyqwsx5wktcd8cc3sq835lu7drv2xwl2wywfgsj5xnu4"
                    , "addr1q62fxv2umyhttkxyxp8x0dlpdt3k6cwng5pxj3jhsydzer3n0d3\
                      \vllmyqwsx5wktcd8cc3sq835lu7drv2xwl2wywfgs4gzg58"
                    ]
            , delegationAddr1Stake1 =
                    [ "addr1qz7svwszky8gcmhrfza7a89z9u0dfzd3l7h23sqlc5yml7en0d3\
                      \vllmyqwsx5wktcd8cc3sq835lu7drv2xwl2wywfgs6r4yq4"
                    , "addr1qw7svwszky8gcmhrfza7a89z9u0dfzd3l7h23sqlc5yml7en0d3\
                      \vllmyqwsx5wktcd8cc3sq835lu7drv2xwl2wywfgsqswdlm"
                    , "addr1q67svwszky8gcmhrfza7a89z9u0dfzd3l7h23sqlc5yml7en0d3\
                      \vllmyqwsx5wktcd8cc3sq835lu7drv2xwl2wywfgs8v2khf"
                    ]
            , delegationAddr1442Stake1 =
                    [ "addr1qrlrt7d6ssjdr6kjykk5xtvcwdhysw3455ukq58mlakmwtpn0d3\
                      \vllmyqwsx5wktcd8cc3sq835lu7drv2xwl2wywfgs4p9yak"
                    , "addr1q0lrt7d6ssjdr6kjykk5xtvcwdhysw3455ukq58mlakmwtpn0d3\
                      \vllmyqwsx5wktcd8cc3sq835lu7drv2xwl2wywfgs0j7dzc"
                    , "addr1qmlrt7d6ssjdr6kjykk5xtvcwdhysw3455ukq58mlakmwtpn0d3\
                      \vllmyqwsx5wktcd8cc3sq835lu7drv2xwl2wywfgsgw6k22"
                    ]
            , pointerAddr0Slot1 =
                    [ "addr1gz2fxv2umyhttkxyxp8x0dlpdt3k6cwng5pxj3jhsydzerspqgpslhplej"
                    , "addr1gw2fxv2umyhttkxyxp8x0dlpdt3k6cwng5pxj3jhsydzerspqgps4acv76"
                    , "addr1g62fxv2umyhttkxyxp8x0dlpdt3k6cwng5pxj3jhsydzerspqgpstr6shz"
                    ]
            , pointerAddr0Slot2 =
                    [ "addr1gz2fxv2umyhttkxyxp8x0dlpdt3k6cwng5pxj3jhsydzer5ph3wczvf22wzcev"
                    , "addr1gw2fxv2umyhttkxyxp8x0dlpdt3k6cwng5pxj3jhsydzer5ph3wczvf2x4v58t"
                    , "addr1g62fxv2umyhttkxyxp8x0dlpdt3k6cwng5pxj3jhsydzer5ph3wczvf2j37qvz"
                    ]
            , mnemonic = [ "test", "walk", "nut", "penalty", "hip", "pave", "soap",
                           "entry", "language", "right", "filter", "choice" ]
            }
        testVectors TestVector
            { rootXPrv =
                    "root_xprv1hretan5mml3tq2p0twkhq4tz4jvka7m2l94kfr6yghkyfar6\
                    \m9wppc7h9unw6p65y23kakzct3695rs32z7vaw3r2lg9scmfj8ec5du3uf\
                    \ydu5yuquxcz24jlkjhsc9vsa4ufzge9s00fn398svhacse5sw90vp2"
            , accXPrv0 =
                    "acct_xprv1crpfhmtv5vdjx9rsqre7z025gartudsanenhart5nh3muqnm\
                    \m9wvu7jr0c2neykydr3r85rw6vht8sg0u4dffts2klnadgk9rt8zh23kfq\
                    \wxrlk3feq0dvvfvd8htlpnhmpu2fpklpl09th296pm7sdc6qafmpwv"
            , accXPrv1 =
                    "acct_xprv1jpfm7zhl6cwr4gkeuhtau603xv898ydam77d7c64xatjcptm\
                    \m9wr5sn95u3a9vudl08q9lwzl4qltal70ddl9qvl3nhcmjkxlan652vgpp\
                    \y8uf42h4y279nxfazumh0hd8hdhkdrlp06qcwe6t69qt236qkr8sxx"
            , addrXPrv0 =
                    "addr_xprv1fzgcl9km0mve2jwe8qxve364w6te9vpddhwpw5g8wnjlupmm\
                    \m9wxpdda6jaglx7smwl6qd5xuzjcweeq8ykp0wg9hng4pg6eumwx2t90sw\
                    \aed7ehsa6j86qsw3fnl4thtemsng6vukmz6ddf3cnd4sfkzuqvwsgp"
            , addrXPub0 =
                    "addr_xpub1fz009r4f0aceaemksezlca9cz8p8rewhaurvyvgg2ndnq9vw\
                    \j3w6lqamjman0pm4y05pqazn8l2hwhnhpx35eedk9566nr3xmtqnv9clhnw7d"
            , addrXPrv1 =
                    "addr_xprv1lq2ylz7fhsn0dfmul2pe833cdwvjnvux9uaxuzaz50gs7pnm\
                    \m9wq343uh5cpfs87tgh9saa86un8e2l266rsge0c5qsmtaud5r64ctndwk\
                    \yth8q07fgusyr3fldhn6lgd5tat5cmcdzvfzhtd0cpsleuxg0f45l9"
            , addrXPub1 =
                    "addr_xpub1y3r70ejyadsaplez83p7uhy8p6l08a5sjl860kszevxu0jax\
                    \cwmx6avghwwqluj3eqg8zn7m0847smgh6hf3hs6ycj9wk6lsrplncvspv5k5u"
            , addrXPrv1442 =
                    "addr_xprv14r6s3v6xu2j33mjlq2ugxf9345sz658rxs9csldztegazrrm\
                    \m9wdsyv8k3df6q2t32ngnvlgvyw8vjy6g5540ap3sxajcrm04ruq9wvgxq\
                    \kej8z8xkt06guqj7wtafqgrstdeukd5pnn8azvhpm2taczzgpnkuuu"
            , addrXPub1442 =
                    "addr_xpub1fqtu4wksrftpdxg5ayacthraykm8eflvnym7lgc4xlzz479d\
                    \ve5csvpdnywywdvkl53cp9uuh6jqs8qkmnevmgr8x06yewrk5hmsyysvsfywe"
            , paymentAddr0  =
                    [ "addr1vpu5vlrf4xkxv2qpwngf6cjhtw542ayty80v8dyr49rf5eg0yu80w"
                    , "addr1vdu5vlrf4xkxv2qpwngf6cjhtw542ayty80v8dyr49rf5eg0m9a08"
                    , "addr1veu5vlrf4xkxv2qpwngf6cjhtw542ayty80v8dyr49rf5eg0n860u"
                    ]
            , paymentAddr1  =
                    [ "addr1vq0a2lgc2e0r597dr983jrf5ns4hxz027u8n7wlcsjcw4ks7qqltj"
                    , "addr1vv0a2lgc2e0r597dr983jrf5ns4hxz027u8n7wlcsjcw4ks7le9tm"
                    , "addr1vc0a2lgc2e0r597dr983jrf5ns4hxz027u8n7wlcsjcw4ks7hmztq"
                    ]
            , paymentAddr1442  =
                    [ "addr1vz8jwared5z7jpcx3znrm70cc7lmhm05g09d94pvvr4mhlsv93pd2"
                    , "addr1vw8jwared5z7jpcx3znrm70cc7lmhm05g09d94pvvr4mhlsv6gmdr"
                    , "addr1v68jwared5z7jpcx3znrm70cc7lmhm05g09d94pvvr4mhlsvj2udc"
                    ]
            , delegationAddr0Stake0 =
                    [ "addr1qpu5vlrf4xkxv2qpwngf6cjhtw542ayty80v8dyr49rf5ewvxwd\
                      \rt70qlcpeeagscasafhffqsxy36t90ldv06wqrk2qwmnp2v"
                    , "addr1qdu5vlrf4xkxv2qpwngf6cjhtw542ayty80v8dyr49rf5ewvxwd\
                      \rt70qlcpeeagscasafhffqsxy36t90ldv06wqrk2q5ggg4z"
                    , "addr1qeu5vlrf4xkxv2qpwngf6cjhtw542ayty80v8dyr49rf5ewvxwd\
                      \rt70qlcpeeagscasafhffqsxy36t90ldv06wqrk2qn5vnas"
                    ]
            , delegationAddr1Stake0 =
                    [ "addr1qq0a2lgc2e0r597dr983jrf5ns4hxz027u8n7wlcsjcw4kkvxwd\
                      \rt70qlcpeeagscasafhffqsxy36t90ldv06wqrk2qedz464"
                    , "addr1qv0a2lgc2e0r597dr983jrf5ns4hxz027u8n7wlcsjcw4kkvxwd\
                      \rt70qlcpeeagscasafhffqsxy36t90ldv06wqrk2qr7eu9m"
                    , "addr1qc0a2lgc2e0r597dr983jrf5ns4hxz027u8n7wlcsjcw4kkvxwd\
                      \rt70qlcpeeagscasafhffqsxy36t90ldv06wqrk2qyza8df"
                    ]
            , delegationAddr1442Stake0 =
                    [ "addr1qz8jwared5z7jpcx3znrm70cc7lmhm05g09d94pvvr4mhlkvxwd\
                      \rt70qlcpeeagscasafhffqsxy36t90ldv06wqrk2qnz7lay"
                    , "addr1qw8jwared5z7jpcx3znrm70cc7lmhm05g09d94pvvr4mhlkvxwd\
                      \rt70qlcpeeagscasafhffqsxy36t90ldv06wqrk2qf39kz2"
                    , "addr1q68jwared5z7jpcx3znrm70cc7lmhm05g09d94pvvr4mhlkvxwd\
                      \rt70qlcpeeagscasafhffqsxy36t90ldv06wqrk2qwdpd2c"
                    ]
            , delegationAddr0Stake1 =
                    [ "addr1qpu5vlrf4xkxv2qpwngf6cjhtw542ayty80v8dyr49rf5e0jz8v\
                      \pqd7krjr3gwsq5rh9rhxjwhdd5qgacdm6ky6p2txqmquepq"
                    , "addr1qdu5vlrf4xkxv2qpwngf6cjhtw542ayty80v8dyr49rf5e0jz8v\
                      \pqd7krjr3gwsq5rh9rhxjwhdd5qgacdm6ky6p2txqpn8s7w"
                    , "addr1qeu5vlrf4xkxv2qpwngf6cjhtw542ayty80v8dyr49rf5e0jz8v\
                      \pqd7krjr3gwsq5rh9rhxjwhdd5qgacdm6ky6p2txqx0rtku"
                    ]
            , delegationAddr1Stake1 =
                    [ "addr1qq0a2lgc2e0r597dr983jrf5ns4hxz027u8n7wlcsjcw4khjz8v\
                      \pqd7krjr3gwsq5rh9rhxjwhdd5qgacdm6ky6p2txqvkdd3e"
                    , "addr1qv0a2lgc2e0r597dr983jrf5ns4hxz027u8n7wlcsjcw4khjz8v\
                      \pqd7krjr3gwsq5rh9rhxjwhdd5qgacdm6ky6p2txqk9kywh"
                    , "addr1qc0a2lgc2e0r597dr983jrf5ns4hxz027u8n7wlcsjcw4khjz8v\
                      \pqd7krjr3gwsq5rh9rhxjwhdd5qgacdm6ky6p2txq3ejlx9"
                    ]
            , delegationAddr1442Stake1 =
                    [ "addr1qz8jwared5z7jpcx3znrm70cc7lmhm05g09d94pvvr4mhlhjz8v\
                      \pqd7krjr3gwsq5rh9rhxjwhdd5qgacdm6ky6p2txqxe38kg"
                    , "addr1qw8jwared5z7jpcx3znrm70cc7lmhm05g09d94pvvr4mhlhjz8v\
                      \pqd7krjr3gwsq5rh9rhxjwhdd5qgacdm6ky6p2txqu22wfx"
                    , "addr1q68jwared5z7jpcx3znrm70cc7lmhm05g09d94pvvr4mhlhjz8v\
                      \pqd7krjr3gwsq5rh9rhxjwhdd5qgacdm6ky6p2txqmkw4p5"
                    ]
            , pointerAddr0Slot1 =
                    [ "addr1gpu5vlrf4xkxv2qpwngf6cjhtw542ayty80v8dyr49rf5egpqgpsjej5ck"
                    , "addr1gdu5vlrf4xkxv2qpwngf6cjhtw542ayty80v8dyr49rf5egpqgpscnt8l7"
                    , "addr1geu5vlrf4xkxv2qpwngf6cjhtw542ayty80v8dyr49rf5egpqgpsxdfmkx"
                    ]
            , pointerAddr0Slot2 =
                    [ "addr1gpu5vlrf4xkxv2qpwngf6cjhtw542ayty80v8dyr49rf5evph3wczvf2jyfghp"
                    , "addr1gdu5vlrf4xkxv2qpwngf6cjhtw542ayty80v8dyr49rf5evph3wczvf27l8yfx"
                    , "addr1geu5vlrf4xkxv2qpwngf6cjhtw542ayty80v8dyr49rf5evph3wczvf22m4sz0"
                    ]
            , mnemonic = [ "art", "forum", "devote", "street", "sure", "rather",
                           "head", "chuckle", "guard", "poverty", "release",
                           "quote", "oak", "craft", "enemy"]
            }
        testVectors TestVector
            { rootXPrv =
                    "root_xprv1az4qjp85qunj75m8krdvdygmv6u4ceqj8vnwaf38wfd69yck\
                    \sa0fwt7n0cfp5zwmht9u0j9dzxxnfssjmkh4vn3dwxvddsle6m2vkm8q8p\
                    \7addwq8y7q3s3eekd3ate40rfr6rpjakctcn2p54cpr3kjmyn4kcps"
            , accXPrv0 =
                    "acct_xprv1vzjuw65sf27s478y8ndz2jpan4rev52h0fdh9z3tdwcplgqk\
                    \sa0wa9wntpw4d74rhj8rs2unf6rfujxu90mstt8ztnjrn80r8sz0edzf22\
                    \jznx2n63qdalqm2tphlfn7k6qxtm0pg85z4ptk639vyjjufccch84r"
            , accXPrv1 =
                    "acct_xprv1uq5jz089v7pjc76lqcac9lkl9d8fntc53h00ajlhr7msm8sk\
                    \sa0xljllgc5c2n5ruklwnkauxvlxsmcwat223rakj5dmfz2j9xxzvmpcqc\
                    \0z9ekuywfhkm8w4k507caj4rv4qjhmnhafs8tkmsx6vtstdujluhmn"
            , addrXPrv0 =
                    "addr_xprv17zj2lhjk379klp40xfzsad0yzygqe45uaggnkzf4ld3emgsk\
                    \sa00ftq88fnfjxg245kjjqcukyjfg4lwmf3r2qqymyyqennch3y8llyg0d\
                    \629pdx0pp0l69lerjz75kxmk5e6cr2d82kafp7a25y0qy5fvvkjv84"
            , addrXPub0 =
                    "addr_xpub1fwgdh5vv6akdc3rjpeq57xxq4lc9m84xcrt6q827mq7u20wu\
                    \w54gs7m552z6v7zzll5tlj8y9afvdhdfn4sx56w4d6jra64gg7qfgjcq4pu8d"
            , addrXPrv1 =
                    "addr_xprv1wz99hznmt96crxthcmnxqttaul6caq4hv5jwttd5lly2mfsk\
                    \sa08q68skn2ggclu6vf40phx3wnj4e8fvxed6at8xxekwa49rg4c3ec8kp\
                    \2nwcxfw6sgxphzckg5v0dausldvya0w6jy5k3cxwrqdjsthqpfkqw6"
            , addrXPub1 =
                    "addr_xpub135hqmkaqydnxnq6wmjkkhasvwjprpnqnzsrwwes6mql45enl\
                    \csqs0vz4xasvja4qsvrw93v3gc7mmep76cf67a4yffdrsvuxqm9qhwqcq4379"
            , addrXPrv1442 =
                    "addr_xprv15pjw7rg5ywgfe4js8dufp6ul4kccpn9gdxtw0hnt4nk2h2gk\
                    \sa09e6p79my6p4k4wggyt482s88zzdhg0fkjgv3ts63z32k9rxc022t005\
                    \4563zytwgcd6x5g8zvsrxkgyldr559ydtz98s3p5tavtdy6ywzxmsd"
            , addrXPub1442 =
                    "addr_xpub1kvsa8pjxlg93nk87tdp08tauy2uc4hdy36xufkjlgdz2dm7p\
                    \6pxx7lftf4zygku3sm5dgswyeqxdvsf768fg2g6ky20pzrgh6ck6f5gyh7uf4"
            , paymentAddr0  =
                    [ "addr1vptvyjfjvs7wdn583rv3th3fvf9fauv5f6gylkhh5k245zckggqj6"
                    , "addr1vdtvyjfjvs7wdn583rv3th3fvf9fauv5f6gylkhh5k245zckh36jn"
                    , "addr1vetvyjfjvs7wdn583rv3th3fvf9fauv5f6gylkhh5k245zcklnajg"
                    ]
            , paymentAddr1  =
                    [ "addr1vr3nq3kyg9c9t4nn6a5zymz3at3zsmcr9lkqxghxh5v822grx6srl"
                    , "addr1v03nq3kyg9c9t4nn6a5zymz3at3zsmcr9lkqxghxh5v822grer2rk"
                    , "addr1vm3nq3kyg9c9t4nn6a5zymz3at3zsmcr9lkqxghxh5v822gr3pdrd"
                    ]
            , paymentAddr1442  =
                    [ "addr1vpdgf8g4dhpmhqwpy2u6nvtmarxczj3qmszc3rhgwpw2tvq5ngy72"
                    , "addr1vddgf8g4dhpmhqwpy2u6nvtmarxczj3qmszc3rhgwpw2tvq5v377r"
                    , "addr1vedgf8g4dhpmhqwpy2u6nvtmarxczj3qmszc3rhgwpw2tvq5yne7c"
                    ]
            , delegationAddr0Stake0 =
                    [ "addr1qptvyjfjvs7wdn583rv3th3fvf9fauv5f6gylkhh5k245zuv4te\
                      \5ey3ksjyq3z0cq8k8pu57rek4qsvpxkc7gyzcnu5qsrl8hh"
                    , "addr1qdtvyjfjvs7wdn583rv3th3fvf9fauv5f6gylkhh5k245zuv4te\
                      \5ey3ksjyq3z0cq8k8pu57rek4qsvpxkc7gyzcnu5q2sywge"
                    , "addr1qetvyjfjvs7wdn583rv3th3fvf9fauv5f6gylkhh5k245zuv4te\
                      \5ey3ksjyq3z0cq8k8pu57rek4qsvpxkc7gyzcnu5qdvq4qt"]
            , delegationAddr1Stake0 =
                    [ "addr1qr3nq3kyg9c9t4nn6a5zymz3at3zsmcr9lkqxghxh5v822vv4te\
                      \5ey3ksjyq3z0cq8k8pu57rek4qsvpxkc7gyzcnu5qcp7mll"
                    , "addr1q03nq3kyg9c9t4nn6a5zymz3at3zsmcr9lkqxghxh5v822vv4te\
                      \5ey3ksjyq3z0cq8k8pu57rek4qsvpxkc7gyzcnu5qzj9jq3"
                    , "addr1qm3nq3kyg9c9t4nn6a5zymz3at3zsmcr9lkqxghxh5v822vv4te\
                      \5ey3ksjyq3z0cq8k8pu57rek4qsvpxkc7gyzcnu5q9wpfgr"
                    ]
            , delegationAddr1442Stake0 =
                    [ "addr1qpdgf8g4dhpmhqwpy2u6nvtmarxczj3qmszc3rhgwpw2tvyv4te\
                      \5ey3ksjyq3z0cq8k8pu57rek4qsvpxkc7gyzcnu5qaglc39"
                    , "addr1qddgf8g4dhpmhqwpy2u6nvtmarxczj3qmszc3rhgwpw2tvyv4te\
                      \5ey3ksjyq3z0cq8k8pu57rek4qsvpxkc7gyzcnu5q8my3wt"
                    , "addr1qedgf8g4dhpmhqwpy2u6nvtmarxczj3qmszc3rhgwpw2tvyv4te\
                      \5ey3ksjyq3z0cq8k8pu57rek4qsvpxkc7gyzcnu5qq8q2xe"
                    ]
            , delegationAddr0Stake1 =
                    [ "addr1qptvyjfjvs7wdn583rv3th3fvf9fauv5f6gylkhh5k245z7uclv\
                      \arqhtzqxk5tc7t9jkmeyawmt2exyacrh6vg5hn47s02df6s"
                    , "addr1qdtvyjfjvs7wdn583rv3th3fvf9fauv5f6gylkhh5k245z7uclv\
                      \arqhtzqxk5tc7t9jkmeyawmt2exyacrh6vg5hn47s4ekq97"
                    , "addr1qetvyjfjvs7wdn583rv3th3fvf9fauv5f6gylkhh5k245z7uclv\
                      \arqhtzqxk5tc7t9jkmeyawmt2exyacrh6vg5hn47sj9jmdv"
                    ]
            , delegationAddr1Stake1 =
                    [ "addr1qr3nq3kyg9c9t4nn6a5zymz3at3zsmcr9lkqxghxh5v822wuclv\
                      \arqhtzqxk5tc7t9jkmeyawmt2exyacrh6vg5hn47s8gv4jc"
                    , "addr1q03nq3kyg9c9t4nn6a5zymz3at3zsmcr9lkqxghxh5v822wuclv\
                      \arqhtzqxk5tc7t9jkmeyawmt2exyacrh6vg5hn47samhudk"
                    , "addr1qm3nq3kyg9c9t4nn6a5zymz3at3zsmcr9lkqxghxh5v822wuclv\
                      \arqhtzqxk5tc7t9jkmeyawmt2exyacrh6vg5hn47s68n89y"
                    ]
            , delegationAddr1442Stake1 =
                    [ "addr1qpdgf8g4dhpmhqwpy2u6nvtmarxczj3qmszc3rhgwpw2tvxuclv\
                      \arqhtzqxk5tc7t9jkmeyawmt2exyacrh6vg5hn47szpdkuz"
                    , "addr1qddgf8g4dhpmhqwpy2u6nvtmarxczj3qmszc3rhgwpw2tvxuclv\
                      \arqhtzqxk5tc7t9jkmeyawmt2exyacrh6vg5hn47scjklrv"
                    , "addr1qedgf8g4dhpmhqwpy2u6nvtmarxczj3qmszc3rhgwpw2tvxuclv\
                      \arqhtzqxk5tc7t9jkmeyawmt2exyacrh6vg5hn47slwjyt7"
                    ]
            , pointerAddr0Slot1 =
                    [ "addr1gptvyjfjvs7wdn583rv3th3fvf9fauv5f6gylkhh5k245zcpqgpsktxqge"
                    , "addr1gdtvyjfjvs7wdn583rv3th3fvf9fauv5f6gylkhh5k245zcpqgpsupln03"
                    , "addr1getvyjfjvs7wdn583rv3th3fvf9fauv5f6gylkhh5k245zcpqgpszla0xf"
                    ]
            , pointerAddr0Slot2 =
                    [ "addr1gptvyjfjvs7wdn583rv3th3fvf9fauv5f6gylkhh5k245zuph3wczvf2sw479c"
                    , "addr1gdtvyjfjvs7wdn583rv3th3fvf9fauv5f6gylkhh5k245zuph3wczvf2u4mjml"
                    , "addr1getvyjfjvs7wdn583rv3th3fvf9fauv5f6gylkhh5k245zuph3wczvf2g3fxsk"
                    ]
            , mnemonic = [ "churn", "shaft", "spoon", "second", "erode", "useless",
                           "thrive", "burst", "group", "seed", "element", "sign",
                           "scrub", "buffalo", "jelly", "grace", "neck", "useless" ]
            }
        testVectors TestVector
            { rootXPrv =
                    "root_xprv17zqw352yj02seytp9apunec55722k93crtplq8chgpfh7cx3\
                    \3dg4v2x3wpyhd9chhkknzhprztumrystkpfl5nyhyeuq0gnwf76r39u9l9\
                    \q3z40hgf5jv6xn8unr5acs3yy8fxg35v5xjsw4kwvf5zfkvc2k9gvr"
            , accXPrv0 =
                    "acct_xprv13rkczfa4xzrax4x0unqnesaasr8s3qnyr2mmjcwga6lrue73\
                    \3dg6at2stlqft9jtz0kk50xsp4yrqnrf9gl78npu50r7e38kpu0f744af4\
                    \6s2r37d7m6sh0uyj9szp86s546k43t2thfw4nzd7zsxckx6ggwkjgx"
            , accXPrv1 =
                    "acct_xprv1rrd6a79s9gktygfmp4gxhme3jxvnxazk45u2twrwfnfg2c73\
                    \3dg323eaegqrxe7lt0zepz3aq3ydgdse8vjs7qks4c4yh0q39pdf4fsv4q\
                    \l5tv5tny3mxzn20r792nl9nk904y0ztqgclyxuxzl56p6fsyg34zms"
            , addrXPrv0 =
                    "addr_xprv1fz8tz0pdda8la0aqhadnzctw0p48zwygkgf4xyar2jjljm73\
                    \3dgkprs4sj8cxfwv9xtfddpdfvjlap0hhg9gd37pr0tp7ue48mh9cnfyy6\
                    \8k52f88z5vghezam30c3pcue6aewl4mqul6nvassxlenh3eqf7zg7r"
            , addrXPub0 =
                    "addr_xpub1x4dme9s2f5xxn77wgjhggqh73r6syy4nvjcdjklnaqrh48f6\
                    \desjgf50dg5jww9gc30j9mhzl3zr3en4mjaltkpel4xempqdln80rjqny9hz0"
            , addrXPrv1 =
                    "addr_xprv18peu0v64maghaa87jvu0txdkftvznq7he2yhntk8eem56mk3\
                    \3dgl2rwt8kmhcgdytr6fjn0t4cdf6sr3xud67yhwnjhzyghgu294f6v0fc\
                    \fzqlactzd8cf5m4tpu7yyn5x58dx6q00d362j6e06g88phjgp2cnfa"
            , addrXPub1 =
                    "addr_xpub1ndtepmpg06x9nskfasvr50mue356e4rqlvuzf8jjcj6n48fe\
                    \exsg7nsjyplmsky60snfh2kreugf8gdgw6d5q77mr5494jl5swwr0ysx0msxw"
            , addrXPrv1442 =
                    "addr_xprv1sr8gz532k2yckskgzv4047taz92905nnr9sysv20ks257ux3\
                    \3dg5000a39wpvw066n3pcgz20062yhqpy9mjjszluq5ef9mg5yjjsseur0\
                    \tu9duqet2zxeqfm9jlt6g4wf3altrupqwggrn00j0f36v8aq8x9v3f"
            , addrXPub1442 =
                    "addr_xpub1vagd70p45qd774cdna2qe2t3t9200hx3jcfvp39w3r9ruscj\
                    \apdncx7hc2mcpjk5ydjqnkt97h532unrm7k8czquss8x7ly7nr5c06qd3aex3"
            , paymentAddr0  =
                    [ "addr1vz83dnlqqtdrlct4kz3f7d07d59w6p4yrtlr62340yklhaqceqmhu"
                    , "addr1vw83dnlqqtdrlct4kz3f7d07d59w6p4yrtlr62340yklhaqcxeph4"
                    , "addr1v683dnlqqtdrlct4kz3f7d07d59w6p4yrtlr62340yklhaqcwmxhw"
                    ]
            , paymentAddr1  =
                    [ "addr1vzr08acccp7s3l9cppvptz7jyflejkkuma2k06vx4vjrcqsy0vmek"
                    , "addr1vwr08acccp7s3l9cppvptz7jyflejkkuma2k06vx4vjrcqsys4pel"
                    , "addr1v6r08acccp7s3l9cppvptz7jyflejkkuma2k06vx4vjrcqsychxey"
                    ]
            , paymentAddr1442  =
                    [ "addr1vz54amjcp4yxynrj877j4nk0umg5w0rf9t5lz3rfprxwjnsz24swj"
                    , "addr1vw54amjcp4yxynrj877j4nk0umg5w0rf9t5lz3rfprxwjnsz4v2wm"
                    , "addr1v654amjcp4yxynrj877j4nk0umg5w0rf9t5lz3rfprxwjnszawdwq"
                    ]
            , delegationAddr0Stake0 =
                    [ "addr1qz83dnlqqtdrlct4kz3f7d07d59w6p4yrtlr62340yklhaxcd4a\
                      \zvtus2m6m3q409pnflcurpnkz3gnxf4ef47ducezs98le8k"
                    , "addr1qw83dnlqqtdrlct4kz3f7d07d59w6p4yrtlr62340yklhaxcd4a\
                      \zvtus2m6m3q409pnflcurpnkz3gnxf4ef47ducezsl5yscc"
                    , "addr1q683dnlqqtdrlct4kz3f7d07d59w6p4yrtlr62340yklhaxcd4a\
                      \zvtus2m6m3q409pnflcurpnkz3gnxf4ef47ducezscgqts2"
                    ]
            , delegationAddr1Stake0 =
                    [ "addr1qzr08acccp7s3l9cppvptz7jyflejkkuma2k06vx4vjrcqkcd4a\
                      \zvtus2m6m3q409pnflcurpnkz3gnxf4ef47ducezshqh0c2"
                    , "addr1qwr08acccp7s3l9cppvptz7jyflejkkuma2k06vx4vjrcqkcd4a\
                      \zvtus2m6m3q409pnflcurpnkz3gnxf4ef47ducezsdnvx8y"
                    , "addr1q6r08acccp7s3l9cppvptz7jyflejkkuma2k06vx4vjrcqkcd4a\
                      \zvtus2m6m3q409pnflcurpnkz3gnxf4ef47ducezs20ga0k"
                    ]
            , delegationAddr1442Stake0 =
                    [ "addr1qz54amjcp4yxynrj877j4nk0umg5w0rf9t5lz3rfprxwjnkcd4a\
                      \zvtus2m6m3q409pnflcurpnkz3gnxf4ef47ducezsezmpfr"
                    , "addr1qw54amjcp4yxynrj877j4nk0umg5w0rf9t5lz3rfprxwjnkcd4a\
                      \zvtus2m6m3q409pnflcurpnkz3gnxf4ef47ducezsr3qgkd"
                    , "addr1q654amjcp4yxynrj877j4nk0umg5w0rf9t5lz3rfprxwjnkcd4a\
                      \zvtus2m6m3q409pnflcurpnkz3gnxf4ef47ducezsydyn7l"
                    ]
            , delegationAddr0Stake1 =
                    [ "addr1qz83dnlqqtdrlct4kz3f7d07d59w6p4yrtlr62340yklhay52ju\
                      \en6vgkzm3s3lzwefszuhhy56rekx68xv33n76a56s8xdg00"
                    , "addr1qw83dnlqqtdrlct4kz3f7d07d59w6p4yrtlr62340yklhay52ju\
                      \en6vgkzm3s3lzwefszuhhy56rekx68xv33n76a56sa4kpsp"
                    , "addr1q683dnlqqtdrlct4kz3f7d07d59w6p4yrtlr62340yklhay52ju\
                      \en6vgkzm3s3lzwefszuhhy56rekx68xv33n76a56s6fj6cn"
                    ]
            , delegationAddr1Stake1 =
                    [ "addr1qzr08acccp7s3l9cppvptz7jyflejkkuma2k06vx4vjrcq552ju\
                      \en6vgkzm3s3lzwefszuhhy56rekx68xv33n76a56s4p97sn"
                    , "addr1qwr08acccp7s3l9cppvptz7jyflejkkuma2k06vx4vjrcq552ju\
                      \en6vgkzm3s3lzwefszuhhy56rekx68xv33n76a56s0j7h0a"
                    , "addr1q6r08acccp7s3l9cppvptz7jyflejkkuma2k06vx4vjrcq552ju\
                      \en6vgkzm3s3lzwefszuhhy56rekx68xv33n76a56sgw6v80"
                    ]
            , delegationAddr1442Stake1 =
                    [ "addr1qz54amjcp4yxynrj877j4nk0umg5w0rf9t5lz3rfprxwjn552ju\
                      \en6vgkzm3s3lzwefszuhhy56rekx68xv33n76a56smrfsp6"
                    , "addr1qw54amjcp4yxynrj877j4nk0umg5w0rf9t5lz3rfprxwjn552ju\
                      \en6vgkzm3s3lzwefszuhhy56rekx68xv33n76a56spsje75"
                    , "addr1q654amjcp4yxynrj877j4nk0umg5w0rf9t5lz3rfprxwjn552ju\
                      \en6vgkzm3s3lzwefszuhhy56rekx68xv33n76a56sxvkzkx"
                    ]
            , pointerAddr0Slot1 =
                    [ "addr1gz83dnlqqtdrlct4kz3f7d07d59w6p4yrtlr62340yklhaqpqgpsvlysw2"
                    , "addr1gw83dnlqqtdrlct4kz3f7d07d59w6p4yrtlr62340yklhaqpqgpsx4arfz"
                    , "addr1g683dnlqqtdrlct4kz3f7d07d59w6p4yrtlr62340yklhaqpqgpsctllq6"
                    ]
            , pointerAddr0Slot2 =
                    [ "addr1gz83dnlqqtdrlct4kz3f7d07d59w6p4yrtlr62340yklhayph3wczvf2cjrgkd"
                    , "addr1gw83dnlqqtdrlct4kz3f7d07d59w6p4yrtlr62340yklhayph3wczvf25fdyg2"
                    , "addr1g683dnlqqtdrlct4kz3f7d07d59w6p4yrtlr62340yklhayph3wczvf2qdlsrr"
                    ]
            , mnemonic = [ "draft", "ability", "female", "child", "jump", "maid",
                           "roof", "hurt", "below", "live", "topple", "paper",
                           "exclude", "ordinary", "coach", "churn", "sunset",
                           "emerge", "blame", "ketchup", "much" ]
            }
        testVectors TestVector
            { rootXPrv =
                    "root_xprv1nzwq05qw3573slqc2he9g68qz0a3hjkexsc0ck3f3gjesqs3\
                    \3az3aprtwwnz0322ky75navetwzk8ve2mpsvqxdz3v9e2vsfe5gmcxzr2j\
                    \9pn432xnghzjc6cgvsx5jwllat5qxy7n7tyqmyjesmv83v5c3ghtcc"
            , accXPrv0 =
                    "acct_xprv1grmww8c9yftkd6nlmuh7wypx46duh8az7sxyg88fr4k3qpc3\
                    \3azk8gc5ntllturxzee5gj2zd5dy48f0ehp6lqudkwvacxjznz8j0mzd2a\
                    \d0t2fmmaystgms97k7maz3afvy0ywjxwk7jzt96cyt43tnqsxye0df"
            , accXPrv1 =
                    "acct_xprv13p9klhdduza8djyvfvwkvz92hnq4sa372kaz60j2a7fjgzg3\
                    \3azlmcm0zv26uldtrl7war6l2cm5ynkplnfqtuylmp52kt9j5yy3ck5vxv\
                    \hqkh9g2xmgnxqay9l58pe683ey4c4tpxq27j75t9wuar224u6fs3jq"
            , addrXPrv0 =
                    "addr_xprv1zq578drze4tp8fq43gur62v2j57vd5nvg862jc8hz5f959g3\
                    \3azuy3hl7ce8cjrxua5nyarqr9f4aatqecjsq9gturmqrcpkwuhu5rchas\
                    \sddgsr6uaa5vzk80fq72wg8fejl9fyc94f8vvp0x904augp5nuw49r"
            , addrXPub0 =
                    "addr_xpub1lytzhygjvgftw9gqazwu0k33zywlc9rx48ey7j9rfel222wj\
                    \6vup0mpq663q84emmgc9vw7jpu5uswnn972jfst2jwccz7v2ltmcsrgulk37x"
            , addrXPrv1 =
                    "addr_xprv1pq9m55k5gwl6kghjmdxxhlvc24nah84e3v7mrcj5d82vjyq3\
                    \3azlqg7n29xrmhumpmnrnqkufxfqasp2f5d9g4ptg060fa5k8csvshmm95\
                    \jmjensafguy23ce0l2rq3cx54u3csmxc6pd7eclu4a5jyqjsnp2xvx"
            , addrXPub1 =
                    "addr_xpub1vh7dfgcpejhgsvf3h9splavunc2lktzq6pywvhd77amer2et\
                    \ywa8ktf9h9n8p6j3cg4r3jl75xprsdfter3pkd35zman3letmfygp9qxjcsku"
            , addrXPrv1442 =
                    "addr_xprv18zk3n3ue6gzwqn6gqyxe4cly6mgfy62q4x9qdz3tketqxrc3\
                    \3azulyvcataalffml635vynej9esk88f6qzvak5dx7tk0fh54e5z95st8s\
                    \7m3mkxfzgujxf4s00k3k9qn3e6rfd34xy4anvxd742e4z30qq6ckqk"
            , addrXPub1442 =
                    "addr_xpub1qlsdp05jtlsyqrxft7cmyjuww0f85ppaknzkszh8q3dh9cwq\
                    \spmqk0pahrhvvjy3eyvntq7ldrv2p8rn5xjmr2vftmxcvma24n29z7qk22fyr"
            , paymentAddr0  =
                    [ "addr1vqy6nhfyks7wdu3dudslys37v252w2nwhv0fw2nfawemmnqsg0y49"
                    , "addr1vvy6nhfyks7wdu3dudslys37v252w2nwhv0fw2nfawemmnqshk74v"
                    , "addr1vcy6nhfyks7wdu3dudslys37v252w2nwhv0fw2nfawemmnqsl5e4h"
                    ]
            , paymentAddr1  =
                    [ "addr1vrhvwtn8sa3duzkm93v5kjjxlv5lvg67j530wyeumngu23cw6qwmf"
                    , "addr1v0hvwtn8sa3duzkm93v5kjjxlv5lvg67j530wyeumngu23cw9e5mq"
                    , "addr1vmhvwtn8sa3duzkm93v5kjjxlv5lvg67j530wyeumngu23cwdmnmm"
                    ]
            , paymentAddr1442  =
                    [ "addr1vz3ca0p33mpqf7gtguf50u34tyu6e60yqvs9hzlv8ehl08s6l9rjc"
                    , "addr1vw3ca0p33mpqf7gtguf50u34tyu6e60yqvs9hzlv8ehl08s6quej3"
                    , "addr1v63ca0p33mpqf7gtguf50u34tyu6e60yqvs9hzlv8ehl08s6g77j2"
                    ]
            , delegationAddr0Stake0 =
                    [ "addr1qqy6nhfyks7wdu3dudslys37v252w2nwhv0fw2nfawemmn8k8tt\
                      \q8f3gag0h89aepvx3xf69g0l9pf80tqv7cve0l33su9wxrs"
                    , "addr1qvy6nhfyks7wdu3dudslys37v252w2nwhv0fw2nfawemmn8k8tt\
                      \q8f3gag0h89aepvx3xf69g0l9pf80tqv7cve0l33sxk40u7"
                    , "addr1qcy6nhfyks7wdu3dudslys37v252w2nwhv0fw2nfawemmn8k8tt\
                      \q8f3gag0h89aepvx3xf69g0l9pf80tqv7cve0l33sp2355v"
                    ]
            , delegationAddr1Stake0 =
                    [ "addr1qrhvwtn8sa3duzkm93v5kjjxlv5lvg67j530wyeumngu23lk8tt\
                      \q8f3gag0h89aepvx3xf69g0l9pf80tqv7cve0l33s8snpj4"
                    , "addr1q0hvwtn8sa3duzkm93v5kjjxlv5lvg67j530wyeumngu23lk8tt\
                      \q8f3gag0h89aepvx3xf69g0l9pf80tqv7cve0l33sarggdm"
                    , "addr1qmhvwtn8sa3duzkm93v5kjjxlv5lvg67j530wyeumngu23lk8tt\
                      \q8f3gag0h89aepvx3xf69g0l9pf80tqv7cve0l33s6lvn9f"
                    ]
            , delegationAddr1442Stake0 =
                    [ "addr1qz3ca0p33mpqf7gtguf50u34tyu6e60yqvs9hzlv8ehl08hk8tt\
                      \q8f3gag0h89aepvx3xf69g0l9pf80tqv7cve0l33sa4jlz7"
                    , "addr1qw3ca0p33mpqf7gtguf50u34tyu6e60yqvs9hzlv8ehl08hk8tt\
                      \q8f3gag0h89aepvx3xf69g0l9pf80tqv7cve0l33s8xfkas"
                    , "addr1q63ca0p33mpqf7gtguf50u34tyu6e60yqvs9hzlv8ehl08hk8tt\
                      \q8f3gag0h89aepvx3xf69g0l9pf80tqv7cve0l33sq6dd4z"
                    ]
            , delegationAddr0Stake1 =
                    [ "addr1qqy6nhfyks7wdu3dudslys37v252w2nwhv0fw2nfawemmnpykh4\
                      \m0f4eyu39ptmnfvzv6c8dnznkfncrravswlzf9gxqlgmzqc"
                    , "addr1qvy6nhfyks7wdu3dudslys37v252w2nwhv0fw2nfawemmnpykh4\
                      \m0f4eyu39ptmnfvzv6c8dnznkfncrravswlzf9gxq9mqtlk"
                    , "addr1qcy6nhfyks7wdu3dudslys37v252w2nwhv0fw2nfawemmnpykh4\
                      \m0f4eyu39ptmnfvzv6c8dnznkfncrravswlzf9gxqz8yshy"
                    ]
            , delegationAddr1Stake1 =
                    [ "addr1qrhvwtn8sa3duzkm93v5kjjxlv5lvg67j530wyeumngu23eykh4\
                      \m0f4eyu39ptmnfvzv6c8dnznkfncrravswlzf9gxqyax93a"
                    , "addr1q0hvwtn8sa3duzkm93v5kjjxlv5lvg67j530wyeumngu23eykh4\
                      \m0f4eyu39ptmnfvzv6c8dnznkfncrravswlzf9gxq7wavwn"
                    , "addr1qmhvwtn8sa3duzkm93v5kjjxlv5lvg67j530wyeumngu23eykh4\
                      \m0f4eyu39ptmnfvzv6c8dnznkfncrravswlzf9gxqejehxp"
                    ]
            , delegationAddr1442Stake1 =
                    [ "addr1qz3ca0p33mpqf7gtguf50u34tyu6e60yqvs9hzlv8ehl083ykh4\
                      \m0f4eyu39ptmnfvzv6c8dnznkfncrravswlzf9gxq7c8mpk"
                    , "addr1qw3ca0p33mpqf7gtguf50u34tyu6e60yqvs9hzlv8ehl083ykh4\
                      \m0f4eyu39ptmnfvzv6c8dnznkfncrravswlzf9gxqytuj7c"
                    , "addr1q63ca0p33mpqf7gtguf50u34tyu6e60yqvs9hzlv8ehl083ykh4\
                      \m0f4eyu39ptmnfvzv6c8dnznkfncrravswlzf9gxqrhcfk2"
                    ]
            , pointerAddr0Slot1 =
                    [ "addr1gqy6nhfyks7wdu3dudslys37v252w2nwhv0fw2nfawemmnqpqgpst4xf0c"
                    , "addr1gvy6nhfyks7wdu3dudslys37v252w2nwhv0fw2nfawemmnqpqgpspll6gs"
                    , "addr1gcy6nhfyks7wdu3dudslys37v252w2nwhv0fw2nfawemmnqpqgpslpaxpg"
                    ]
            , pointerAddr0Slot2 =
                    [ "addr1gqy6nhfyks7wdu3dudslys37v252w2nwhv0fw2nfawemmnyph3wczvf2ff5mz3"
                    , "addr1gvy6nhfyks7wdu3dudslys37v252w2nwhv0fw2nfawemmnyph3wczvf29j6huk"
                    , "addr1gcy6nhfyks7wdu3dudslys37v252w2nwhv0fw2nfawemmnyph3wczvf23kgrhl"
                    ]
            , mnemonic = [ "excess", "behave", "track", "soul", "table", "wear",
                           "ocean", "cash", "stay", "nature", "item", "turtle",
                           "palm", "soccer", "lunch", "horror", "start", "stumble",
                           "month", "panic", "right", "must", "lock", "dress" ]
            }

{-------------------------------------------------------------------------------
                                 Properties
-------------------------------------------------------------------------------}

prop_publicChildKeyDerivation
    :: (SomeMnemonic, SndFactor)
    -> Role
    -> Index 'Soft 'PaymentK
    -> Property
prop_publicChildKeyDerivation (mw, (SndFactor sndFactor)) cc ix =
    addrXPub1 === addrXPub2
  where
    rootXPrv = genMasterKeyFromMnemonic mw sndFactor :: Shelley 'RootK XPrv
    accXPrv  = deriveAccountPrivateKey rootXPrv minBound
    addrXPub1 = toXPub <$> deriveAddressPrivateKey accXPrv cc ix
    addrXPub2 = deriveAddressPublicKey (toXPub <$> accXPrv) cc ix

prop_publicMultisigForPaymentDerivation
    :: (SomeMnemonic, SndFactor)
    -> Index 'Soft 'PaymentK
    -> Property
prop_publicMultisigForPaymentDerivation (mw, (SndFactor sndFactor)) ix =
    multisigXPub1 === multisigXPub2
  where
    rootXPrv = genMasterKeyFromMnemonic mw sndFactor :: Shelley 'RootK XPrv
    accXPrv  = deriveAccountPrivateKey rootXPrv minBound
    multisigXPub1 = toXPub <$> deriveMultisigForPaymentPrivateKey accXPrv ix
    multisigXPub2 = deriveMultisigForPaymentPublicKey (toXPub <$> accXPrv) ix

prop_publicMultisigForDelegationDerivation
    :: (SomeMnemonic, SndFactor)
    -> Index 'Soft 'PaymentK
    -> Property
prop_publicMultisigForDelegationDerivation (mw, (SndFactor sndFactor)) ix =
    multisigXPub1 === multisigXPub2
  where
    rootXPrv = genMasterKeyFromMnemonic mw sndFactor :: Shelley 'RootK XPrv
    accXPrv  = deriveAccountPrivateKey rootXPrv minBound
    multisigXPub1 = toXPub <$> deriveMultisigForDelegationPrivateKey accXPrv ix
    multisigXPub2 = deriveMultisigForDelegationPublicKey (toXPub <$> accXPrv) ix

prop_accountKeyDerivation
    :: (SomeMnemonic, SndFactor)
    -> Index 'Hardened 'AccountK
    -> Property
prop_accountKeyDerivation (mw, (SndFactor sndFactor)) ix =
    accXPrv `seq` property ()
  where
    rootXPrv = genMasterKeyFromMnemonic mw sndFactor :: Shelley 'RootK XPrv
    accXPrv = deriveAccountPrivateKey rootXPrv ix

prop_toEnumRole :: Int -> Property
prop_toEnumRole n =
    n > fromEnum UTxOInternal ==> expectFailure $ property $
        (toEnum n :: Role) `seq` ()

prop_roundtripEnumRole :: Role -> Property
prop_roundtripEnumRole ix =
    (toEnum . fromEnum) ix === ix

prop_pointerAddressConstruction
    :: (SomeMnemonic, SndFactor)
    -> Role
    -> Index 'Soft 'PaymentK
    -> NetworkDiscriminant Shelley
    -> ChainPointer
    -> Property
prop_pointerAddressConstruction (mw, (SndFactor sndFactor)) cc ix net ptr =
    pointerAddr `seq` property ()
  where
    rootXPrv = genMasterKeyFromMnemonic mw sndFactor :: Shelley 'RootK XPrv
    accXPrv  = deriveAccountPrivateKey rootXPrv minBound
    addrXPub = toXPub <$> deriveAddressPrivateKey accXPrv cc ix
    pointerAddr = pointerAddress net (PaymentFromKey addrXPub) ptr


{-------------------------------------------------------------------------------
                             Golden tests
-------------------------------------------------------------------------------}
-- | Definitions: Let's assume we have private key         < xprv | pub | cc >
-- Then, extended private key should be understood as      < xprv |     | cc >
--       extended public key  should be understood as      <      | pub | cc >
--       verification public key should be understood as   <      | pub |    >

data GoldenTestPointerAddress = GoldenTestPointerAddress
    {
      -- | The verification public key as string to be encoded into base16 form
      -- After that it should have 32-byte length
       verKey :: Text

      -- | Location of stake cerificate in the blockchain
    ,  stakePtr :: ChainPointer

      -- | Network tag
    ,  netTag :: Integer

      -- | Expected address as encoded into base16 form
    ,  expectedAddr :: Text
    }

goldenTestPointerAddress :: GoldenTestPointerAddress -> SpecWith ()
goldenTestPointerAddress GoldenTestPointerAddress{..} =
    it ("pointer address for networkId " <> show netTag) $ do
        let (Just xPub) = xpubFromBytes $ b16encode $ T.append verKey verKey
        let addrXPub = liftXPub xPub :: Shelley 'PaymentK XPub
        let (Right tag) = mkNetworkDiscriminant netTag
        let ptrAddr = pointerAddress tag (PaymentFromKey addrXPub) stakePtr
        let (Right bytes) = b16decode expectedAddr
        ptrAddr `shouldBe` unsafeMkAddress bytes

data GoldenTestEnterpriseAddress = GoldenTestEnterpriseAddress
    {
      -- | The verification public key as string to be encoded into base16 form
      -- After that it should have 32-byte length
       verKey :: Text

      -- | Network tag
    ,  netTag :: Integer

      -- | Expected address as encoded into base16 form
    ,  expectedAddr :: Text
    }

goldenTestEnterpriseAddress :: GoldenTestEnterpriseAddress -> SpecWith ()
goldenTestEnterpriseAddress GoldenTestEnterpriseAddress{..} =
    it ("enterprise address for networkId " <> show netTag) $ do
        let (Just xPub) = xpubFromBytes $ b16encode $ T.append verKey verKey
        let addrXPub = liftXPub xPub :: Shelley 'PaymentK XPub
        let (Right tag) = mkNetworkDiscriminant netTag
        let enterpriseAddr = Shelley.paymentAddress tag (PaymentFromKey addrXPub)
        let (Right bytes) = b16decode expectedAddr
        enterpriseAddr `shouldBe` unsafeMkAddress bytes

data GoldenTestBaseAddress = GoldenTestBaseAddress
    {
      -- | The verification public key as string to be encoded into base16 form
      -- After that it should have 32-byte length
       verKeyPayment :: Text

      -- | The verification public key as string to be encoded into base16 form
      -- After that it should have 32-byte length
    ,  verKeyStake :: Text

      -- | Network tag
    ,  netTag :: Integer

      -- | Expected address as encoded into base16 form
    ,  expectedAddr :: Text
    }

goldenTestBaseAddress :: GoldenTestBaseAddress -> SpecWith ()
goldenTestBaseAddress GoldenTestBaseAddress{..} =
    it ("base address for networkId " <> show netTag) $ do
        let (Just xPub1) =
                xpubFromBytes $ b16encode $ T.append verKeyPayment verKeyPayment
        let addrXPub = liftXPub xPub1 :: Shelley 'PaymentK XPub
        let (Just xPub2) =
                xpubFromBytes $ b16encode $ T.append verKeyStake verKeyStake
        let stakeXPub = liftXPub xPub2 :: Shelley 'DelegationK XPub
        let (Right tag) = mkNetworkDiscriminant netTag
        let baseAddr = delegationAddress tag (PaymentFromKey addrXPub) (DelegationFromKey stakeXPub)
        let (Right bytes) = b16decode expectedAddr
        baseAddr `shouldBe` unsafeMkAddress bytes

data TestVector = TestVector
    {
      -- | The extended root private key, bech32 encoded prefixed with 'root_xprv'
      rootXPrv :: Text

      -- | The extended 0th account private key, bech32 encoded prefixed with 'acct_xprv'
    , accXPrv0 :: Text

      -- | The extended 1st account private key, bech32 encoded prefixed with 'acct_xprv'
    , accXPrv1 :: Text

      -- | The extended 0th address private key, bech32 encoded prefixed with 'addr_xprv'
    , addrXPrv0 :: Text

      -- | The extended 0th address public key, bech32 encoded prefixed with 'addr_xpub'
    , addrXPub0 :: Text

      -- | The extended 1st address private key, bech32 encoded prefixed with 'addr_xprv'
    , addrXPrv1 :: Text

      -- | The extended 1st address public key, bech32 encoded prefixed with 'addr_xpub'
    , addrXPub1 :: Text

      -- | The extended 1442nd address private key, bech32 encoded prefixed with 'addr_xprv'
    , addrXPrv1442 :: Text

      -- | The extended 1442nd address public key, bech32 encoded prefixed with 'addr_xpub'
    , addrXPub1442 :: Text

      -- | The payment address for 0th address key, with a networking tag 0, 3 and 6, respectively.
      -- Each bech32 encoded prefixed with 'addr'
    , paymentAddr0 :: [Text]

      -- | The payment address for 1st address key, with a networking tag 0, 3 and 6, respectively.
      -- Each bech32 encoded prefixed with 'addr'
    , paymentAddr1 :: [Text]

      -- | The payment address for 1442nd address key, with a networking tag 0, 3 and 6, respectively.
      -- Each bech32 encoded prefixed with 'addr'
    , paymentAddr1442 :: [Text]

      -- | Delegation addresses for the 0th address key and the 0th account delegation key,
      -- with a networking tag 0, 3 and 6, respectively. Each bech32 encoded prefixed with 'addr'
     , delegationAddr0Stake0 :: [Text]

      -- | Delegation addresses for the 1st address key and the 0th account delegation key,
      -- with a networking tag 0, 3 and 6, respectively. Each bech32 encoded prefixed with 'addr'
     , delegationAddr1Stake0 :: [Text]

      -- | Delegation addresses for the 1442nd address key and the 0th account delegation key,
      -- with a networking tag 0, 3 and 6, respectively. Each bech32 encoded prefixed with 'addr'
     , delegationAddr1442Stake0 :: [Text]

      -- | Delegation addresses for the 0th address key and the 1st account delegation key,
      -- with a networking tag 0, 3 and 6, respectively. Each bech32 encoded prefixed with 'addr'
     , delegationAddr0Stake1 :: [Text]

      -- | Delegation addresses for the 1st address key and the 1st account delegation key,
      -- with a networking tag 0, 3 and 6, respectively. Each bech32 encoded prefixed with 'addr'
     , delegationAddr1Stake1 :: [Text]

      -- | Delegation addresses for the 1442nd address key and the 1st account delegation key,
      -- with a networking tag 0, 3 and 6, respectively. Each bech32 encoded prefixed with 'addr'
     , delegationAddr1442Stake1 :: [Text]

      -- | Pointer addresses for the 0th address key and the delegation certificate located in slot 1,
      -- transaction index 2, certificte list index 3, with a networking tag 0, 3 and 6, respectively.
      -- Each bech32 encoded prefixed with 'addr'
     , pointerAddr0Slot1 :: [Text]

      -- | Pointer addresses for the 0th address key and the delegation certificate located in slot 24157,
      -- transaction index 177, certificte list index 42, with a networking tag 0, 3 and 6, respectively.
      -- Each bech32 encoded prefixed with 'addr'
     , pointerAddr0Slot2 :: [Text]

      -- | Corresponding Mnemonic
    , mnemonic :: [Text]
    }

testVectors :: TestVector -> SpecWith ()
testVectors TestVector{..} = it (show $ T.unpack <$> mnemonic) $ do
    let (Right mw) = mkSomeMnemonic @'[9,12,15,18,21,24] mnemonic
    let sndFactor = mempty
    let rootK = genMasterKeyFromMnemonic mw sndFactor :: Shelley 'RootK XPrv
    let (Right hrpRoot) = Bech32.humanReadablePartFromText "root_xprv"
    let rootXPrv' = bech32With hrpRoot $ getExtendedKeyAddr rootK
    rootXPrv' `shouldBe` rootXPrv

    let (Right hrp) = Bech32.humanReadablePartFromText "acct_xprv"
    let Just accIx0 = hardenedIndex 0x80000000
    let acctK0 = deriveAccountPrivateKey rootK accIx0
    let accXPrv0' = bech32With hrp $ getExtendedKeyAddr acctK0
    accXPrv0' `shouldBe` accXPrv0
    let Just accIx1 = hardenedIndex 0x80000001
    let acctK1 = deriveAccountPrivateKey rootK accIx1
    let accXPrv1' = bech32With hrp $ getExtendedKeyAddr acctK1
    accXPrv1' `shouldBe` accXPrv1

    let (Right hrpPrv) = Bech32.humanReadablePartFromText "addr_xprv"
    let (Right hrpPub) = Bech32.humanReadablePartFromText "addr_xpub"
    let Just addIx0 = softIndex 0x00000000
    let addrK0prv = deriveAddressPrivateKey acctK0 UTxOExternal addIx0
    let addrXPrv0' = bech32With hrpPrv $ getExtendedKeyAddr addrK0prv
    addrXPrv0' `shouldBe` addrXPrv0
    let addrXPub0' = bech32With hrpPub $ getPublicKeyAddr $ toXPub <$> addrK0prv
    addrXPub0' `shouldBe` addrXPub0

    let Just addIx1 = softIndex 0x00000001
    let addrK1prv = deriveAddressPrivateKey acctK0 UTxOExternal addIx1
    let addrXPrv1' = bech32With hrpPrv $ getExtendedKeyAddr addrK1prv
    addrXPrv1' `shouldBe` addrXPrv1
    let addrXPub1' = bech32With hrpPub $ getPublicKeyAddr $ toXPub <$> addrK1prv
    addrXPub1' `shouldBe` addrXPub1
    let Just addIx1442 = softIndex 0x000005a2
    let addrK1442prv = deriveAddressPrivateKey acctK0 UTxOExternal addIx1442
    let addrXPrv1442' = bech32With hrpPrv $ getExtendedKeyAddr addrK1442prv
    addrXPrv1442' `shouldBe` addrXPrv1442
    let addrXPub1442' = bech32With hrpPub $ getPublicKeyAddr $ toXPub <$> addrK1442prv
    addrXPub1442' `shouldBe` addrXPub1442

    let networkTags = rights $ mkNetworkDiscriminant <$> [0,3,6]
    let paymentAddr0' = getPaymentAddr addrK0prv <$> networkTags
    paymentAddr0' `shouldBe` paymentAddr0
    let paymentAddr1' = getPaymentAddr addrK1prv <$> networkTags
    paymentAddr1' `shouldBe` paymentAddr1
    let paymentAddr1442' = getPaymentAddr addrK1442prv <$> networkTags
    paymentAddr1442' `shouldBe` paymentAddr1442

    let slot1 = ChainPointer 1 2 3
    let pointerAddr0Slot1' = getPointerAddr addrK0prv slot1 <$> networkTags
    pointerAddr0Slot1' `shouldBe` pointerAddr0Slot1
    let slot2 = ChainPointer 24157 177 42
    let pointerAddr0Slot2' = getPointerAddr addrK0prv slot2 <$> networkTags
    pointerAddr0Slot2' `shouldBe` pointerAddr0Slot2

    let stakeKPub0 = toXPub <$> deriveDelegationPrivateKey acctK0
    let delegationAddr0Stake0' = getDelegationAddr addrK0prv (DelegationFromKey stakeKPub0) <$> networkTags
    delegationAddr0Stake0' `shouldBe` delegationAddr0Stake0
    let delegationAddr1Stake0' = getDelegationAddr addrK1prv (DelegationFromKey stakeKPub0) <$> networkTags
    delegationAddr1Stake0' `shouldBe` delegationAddr1Stake0
    let delegationAddr1442Stake0' = getDelegationAddr addrK1442prv (DelegationFromKey stakeKPub0) <$> networkTags
    delegationAddr1442Stake0' `shouldBe` delegationAddr1442Stake0
    let stakeKPub1 = toXPub <$> deriveDelegationPrivateKey acctK1
    let delegationAddr0Stake1' = getDelegationAddr addrK0prv (DelegationFromKey stakeKPub1) <$> networkTags
    delegationAddr0Stake1' `shouldBe` delegationAddr0Stake1
    let delegationAddr1Stake1' = getDelegationAddr addrK1prv (DelegationFromKey stakeKPub1) <$> networkTags
    delegationAddr1Stake1' `shouldBe` delegationAddr1Stake1
    let delegationAddr1442Stake1' = getDelegationAddr addrK1442prv (DelegationFromKey stakeKPub1) <$> networkTags
    delegationAddr1442Stake1' `shouldBe` delegationAddr1442Stake1
  where
    getExtendedKeyAddr = unsafeMkAddress . xprvToBytes . getKey
    getPublicKeyAddr = unsafeMkAddress . xpubToBytes . getKey
    getPaymentAddr addrKPrv net =  bech32 $ paymentAddress net (PaymentFromKey (toXPub <$> addrKPrv))
    getPointerAddr addrKPrv ptr net =  bech32 $ pointerAddress net (PaymentFromKey (toXPub <$> addrKPrv)) ptr
    getDelegationAddr addrKPrv stakeKPub net =
        bech32 $ delegationAddress net (PaymentFromKey (toXPub <$> addrKPrv)) stakeKPub

prop_roundtripTextEncoding
    :: (Address -> Text)
        -- ^ encode to 'Text'
    -> (Text -> Maybe Address)
        -- ^ decode from 'Text'
    -> Shelley 'PaymentK XPub
        -- ^ An arbitrary public key
    -> NetworkDiscriminant Shelley
        -- ^ An arbitrary network discriminant
    -> Property
prop_roundtripTextEncoding encode' decode addXPub discrimination =
    (result == pure address)
        & counterexample (unlines
            [ "Address " <> T.unpack (encode' address)
            , "↳       " <> maybe "ø" (T.unpack . encode') result
            ])
        & label (show $ addressDiscrimination @Shelley discrimination)
  where
    address = paymentAddress discrimination (PaymentFromKey addXPub)
    result  = decode (encode' address)

prop_roundtripTextEncodingDelegation
    :: (Address -> Text)
        -- ^ encode to 'Text'
    -> (Text -> Maybe Address)
        -- ^ decode from 'Text'
    -> Shelley 'PaymentK XPub
        -- ^ An arbitrary address public key
    -> Shelley 'DelegationK XPub
        -- ^ An arbitrary delegation public key
    -> NetworkDiscriminant Shelley
        -- ^ An arbitrary network discriminant
    -> Property
prop_roundtripTextEncodingDelegation encode' decode addXPub delegXPub discrimination =
    (result == pure address)
        & counterexample (unlines
            [ "Address " <> T.unpack (encode' address)
            , "↳       " <> maybe "ø" (T.unpack . encode') result
            ])
        & label (show $ addressDiscrimination @Shelley discrimination)
  where
    address = delegationAddress discrimination (PaymentFromKey addXPub) (DelegationFromKey delegXPub)
    result  = decode (encode' address)

prop_roundtripTextEncodingPointer
    :: (Address -> Text)
        -- ^ encode to 'Text'
    -> (Text -> Maybe Address)
        -- ^ decode from 'Text'
    -> Shelley 'PaymentK XPub
        -- ^ An arbitrary address public key
    -> ChainPointer
        -- ^ An arbitrary delegation key locator
    -> NetworkDiscriminant Shelley
        -- ^ An arbitrary network discriminant
    -> Property
prop_roundtripTextEncodingPointer encode' decode addXPub ptr discrimination =
    (result == pure address)
        & counterexample (unlines
            [ "Address " <> T.unpack (encode' address)
            , "↳       " <> maybe "ø" (T.unpack . encode') result
            ])
        & label (show $ addressDiscrimination @Shelley discrimination)
  where
    address = pointerAddress discrimination (PaymentFromKey addXPub) ptr
    result  = decode (encode' address)

{-------------------------------------------------------------------------------
                             Arbitrary Instances
-------------------------------------------------------------------------------}

newtype SndFactor = SndFactor ScrubbedBytes
    deriving stock (Eq, Show)
    deriving newtype (ByteArrayAccess)

instance Arbitrary SndFactor where
    arbitrary = do
        n <- choose (0, 64)
        bytes <- BS.pack <$> vector n
        return $ SndFactor $ BA.convert bytes

b16encode :: Text -> ByteString
b16encode = encode EBase16 . T.encodeUtf8

b16decode :: Text -> Either String ByteString
b16decode = fromBase16 . T.encodeUtf8

instance Arbitrary Role where
    shrink _ = []
    arbitrary = arbitraryBoundedEnum
