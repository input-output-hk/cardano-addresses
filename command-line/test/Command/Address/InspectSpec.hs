{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Command.Address.InspectSpec
    ( spec
    ) where

import Prelude

import Control.Monad
    ( forM_ )
import Test.Hspec
    ( Spec, SpecWith, expectationFailure, it, shouldBe, shouldContain )
import Test.Utils
    ( SchemaRef, cli, describeCmd, validateJSON )

import qualified Data.Aeson as Json
import qualified Data.ByteString.Lazy.Char8 as BL8

spec :: Spec
spec = describeCmd [ "address", "inspect" ] $ do
    specInspectAddress ["Byron", "none"] []
        "37btjrVyb4KEgoGCHJ7XFaJRLBRiVuvcrQWPpp4HeaxdTxhKwQjXHNKL4\
        \3NhXaQNa862BmxSFXZFKqPqbxRc3kCUeTRMwjJevFeCKokBG7A7num5Wh"

    specInspectAddress ["Byron", "address_index", "account_index"]
        [ "--root"
        , "root_xvk18amv7cs8kj0mxpk0l3vk2w6g22vyf7y5texr9huevqg9kd3dav\
          \gv5j52xrfcf90kxx2zdrrl826pzc2kptgwegzzzpfgddwqkrk2gpclvvx76"
        ]
        "DdzFFzCqrht5csm2GKhnVrjzKpVHHQFNXUDhAFDyLWVY5w8ZsJRP2uhwZ\
        \q2CEAVzDZXYXa4GvggqYEegQsdKAKikFfrrCoHheLH2Jskr"

    specInspectAddress ["Icarus", "none"] []
        "Ae2tdPwUPEYz6ExfbWubiXPB6daUuhJxikMEb4eXRp5oKZBKZwrbJ2k7EZe"

    specInspectAddress ["Shelley", "none"] []
        "addr1vpu5vlrf4xkxv2qpwngf6cjhtw542ayty80v8dyr49rf5eg0yu80w"

    specInspectAddress ["Shelley", "by value"] []
        "addr1qdu5vlrf4xkxv2qpwngf6cjhtw542ayty80v8dyr49rf5ew\
        \vxwdrt70qlcpeeagscasafhffqsxy36t90ldv06wqrk2q5ggg4z"

    specInspectAddress ["Shelley", "by pointer"] []
        "addr1gw2fxv2umyhttkxyxp8x0dlpdt3k6cwng5pxj3jhsydzer5ph3wczvf2x4v58t"

    -- reward account: keyhash28
    specInspectAddress ["Shelley", "by value", "stake_key_hash"] []
        "stake1upshvetj09hxjcm9v9jxgunjv4ehxmr0d3hkcmmvdakx7mqcjv83c"
    specInspectAddress ["Shelley", "by value", "stake_key_hash_bech32"] []
        "stake1upshvetj09hxjcm9v9jxgunjv4ehxmr0d3hkcmmvdakx7mqcjv83c"

    -- reward account: scripthash28
    specInspectAddress ["Shelley", "by value", "script_hash"] []
        "stake17pshvetj09hxjcm9v9jxgunjv4ehxmr0d3hkcmmvdakx7mq36s8xc"
    specInspectAddress ["Shelley", "by value", "script_hash_bech32"] []
        "stake17pshvetj09hxjcm9v9jxgunjv4ehxmr0d3hkcmmvdakx7mq36s8xc"

    -- cardano-cli generated --testnet-magic 42 addresses
    specInspectAddress ["Shelley", "by value", "stake_key_hash", "spending_key_hash"] []
        "addr_test1qpwr8l57ceql23ylyprl6qgct239lxph8clwxy5w8r4qdz8ct9uut5a\
        \hmxqkgwy9ecn5carsv39frsgsq09u70wmqwhqjqcjqs"
    specInspectAddress ["Shelley", "by value", "stake_key_hash"] []
        "stake_test1uru9j7w96wmanqty8zzuuf6vw3cxgj53cygq8j708hds8tsntl0j7"
    specInspectAddress ["Shelley", "by value", "stake_key_hash_bech32"] []
        "stake_test1uru9j7w96wmanqty8zzuuf6vw3cxgj53cygq8j708hds8tsntl0j7"

    -- cardano-cli generated --mainnet addresses
    specInspectAddress ["Shelley", "by value", "stake_key_hash", "spending_key_hash"] []
        "addr1q9777p2w2hqa3cl0ah97pdwyavjnpf0ex3muvqgttavjxhku2rp98h9drzkdf\
        \va8ea775jszmd799k59aknpvqyn6wwqwll7uw"
    specInspectAddress ["Shelley", "by value", "stake_key_hash"] []
        "stake1u8w9psjnmjk33tx5kwnu7l02fgpdklzjm2z7mfskqzfa88qsjpk8l"
    specInspectAddress ["Shelley", "by value", "stake_key_hash_bech32"] []
        "stake1u8w9psjnmjk33tx5kwnu7l02fgpdklzjm2z7mfskqzfa88qsjpk8l"

    specInspectMalformed
        "💩"

    specInspectInvalid "Wrong input size of 28" []
        "79467c69a9ac66280174d09d62575ba955748b21dec3b483a9469a65"

    -- 32-byte long script hash
    specInspectInvalid "Unknown address type" []
        "stake17pshvetj09hxjcm9v9jxgunjv4ehxmr0d3hkcmmvdakx7mrgdp5xscfm7jc"

    -- Provided key is not the root key.
    specInspectInvalid "Failed to decrypt derivation path"
        [ "--root"
        , "root_xvk1kvz64d7yggggk5uc8kf8t8jjmh3djlx7ksr2xu25na5ypjzjs5\
          \9j8ym5gqga8un7yg8e6et6sex8kx0cejwjtz8gh8pj0zg7kc53nuqd92dr7"
        ]
        "DdzFFzCqrht5csm2GKhnVrjzKpVHHQFNXUDhAFDyLWVY5w8ZsJRP2uhwZ\
        \q2CEAVzDZXYXa4GvggqYEegQsdKAKikFfrrCoHheLH2Jskr"

    -- Invalid CRC
    specInspectInvalid "non-matching crc32" []
        "Ae2tdPwUPEZ5QJkfzoJgarugsX3rUVbTjg8nqTYmuy2c2msy5augpnm91ZR"

specInspectAddress :: [String] -> [String] -> String -> SpecWith ()
specInspectAddress mustHave args addr = it addr $ do
    (out, err) <- cli ([ "address", "inspect" ] <> args) addr
    err `shouldBe` ("" :: String)
    case Json.eitherDecode (BL8.pack out) of
        Left e -> expectationFailure $ "malformed JSON: " <> show e
        Right json -> validateJSON schema json >>= \case
            [] -> forM_ mustHave (shouldContain out)
            es -> expectationFailure $ "invalid JSON: " <> unlines (show <$> es)
  where
    schema :: SchemaRef
    schema = "./schemas/address-inspect.json"

specInspectMalformed :: String -> SpecWith ()
specInspectMalformed str = it ("malformed: " <> str) $ do
    (out, err) <- cli [ "address", "inspect" ] str
    out `shouldBe` ("" :: String)
    err `shouldContain` "Couldn't detect input encoding?"

specInspectInvalid :: String -> [String] -> String -> SpecWith ()
specInspectInvalid errstr args str = it ("invalid: " <> str) $ do
    (out, err) <- cli ([ "address", "inspect" ] <> args) str
    out `shouldBe` ("" :: String)
    err `shouldContain` errstr
