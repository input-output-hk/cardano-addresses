{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

{-# OPTIONS_HADDOCK hide #-}

module Command.Key.Child
    ( Cmd (..)
    , mod
    , run
    ) where

import Prelude hiding
    ( mod )

import Cardano.Address.Derivation
    ( DerivationScheme (..), Index(..), deriveXPrv, deriveXPub, xprvToBytes, xpubToBytes )
import Codec.Binary.Encoding
    ( AbstractEncoding (..) )
import Control.Monad
    ( foldM )
import Data.Functor.Identity
    ( Identity (..) )
import Options.Applicative
    ( CommandFields, Mod, command, footerDoc, helper, info, progDesc )
import Options.Applicative.Derivation
    ( DerivationPath, castDerivationPath, derivationPathArg )
import Options.Applicative.Help.Pretty
    ( string )
import System.IO
    ( stdin, stdout )
import System.IO.Extra
    ( hGetXP__, hPutBytes )

import qualified Cardano.Codec.Bech32.Prefixes as CIP5

newtype Cmd = Child
    { path :: DerivationPath
    } deriving (Show)

mod :: (Cmd -> parent) -> Mod CommandFields parent
mod liftCmd = command "child" $
    info (helper <*> fmap liftCmd parser) $ mempty
        <> progDesc "Derive child keys from a parent public/private key"
        <> footerDoc (Just $ string $ mconcat
            [ "The parent key is read from stdin."
            ])
  where
    parser = Child
        <$> derivationPathArg

run :: Cmd -> IO ()
run Child{path} = do
    (hrp, child) <- hGetXP__ stdin allowedPrefixes >>= \case
        Left (hrp, xpub) -> do
            let ixs = castDerivationPath path
            case foldM (deriveXPub DerivationScheme2) xpub ixs of
                Just child ->
                    (,xpubToBytes child) <$> childHrpFor (getIndex <$> ixs) hrp
                Nothing ->
                    fail
                        "Couldn't derive child key. If you're trying to derive \
                        \children on a PUBLIC key, you must use soft indexes only."

        Right (hrp, xprv) -> do
            let ixs = castDerivationPath path
            let scheme = if length ixs == 2 && hrp == CIP5.root_xsk
                    then DerivationScheme1
                    else DerivationScheme2
            let Identity child = foldM (\k -> pure . deriveXPrv scheme k) xprv ixs
            (,xprvToBytes child) <$> childHrpFor (getIndex <$> ixs) hrp

    hPutBytes stdout child (EBech32 hrp)
  where
    allowedPrefixes =
        [ CIP5.root_xsk
        , CIP5.acct_xsk
        , CIP5.acct_xvk
        ]

    -- As a reminder, we really have two scenarios:
    --
    -- Byron Legacy:
    --
    --     m / rnd_account' / rnd_address'
    --
    --
    -- Icarus & Shelley:
    --
    --     m / purpose' / coin_type' / account' / role / index
    --
    -- We do not allow derivations to anywhere in the path to avoid people
    -- shooting themselves in the foot. Hence We only allow the following
    -- transformations:
    --
    -- root_xsk => addr_xsk: (legacy)
    --     m => m / rnd_account' / rnd_address
    --
    -- root_xsk => acct_xsk: (hard derivation from root to account)
    --     m => m / purpose' / coin_type' / account'
    --
    -- root_xsk => acct_xsk: (hard derivation from root to address)
    --     m => m / purpose' / coin_type' / account' / role / index
    --
    -- acct_xsk => addr_xsk: (hard derivation from account to address)
    --     m / purpose' / coin_type' / account' => m / purpose' / coin_type' / account' / role / index
    --
    -- acct_xvk => addr_xvk: (soft derivation from account to address)
    --     m / purpose' / coin_type' / account' => m / purpose' / coin_type' / account' / role / index
    --
    -- There's no use-case at the moment for accessing intermediate paths such
    -- as m / purpose' or m / purpose' / coin_type' so we do not expose them.
    childHrpFor [_,_,_,2,_] hrp
        | hrp == CIP5.root_xsk = pure CIP5.stake_xsk

    childHrpFor [_,_,_,3,_] hrp
        | hrp == CIP5.root_xsk = pure CIP5.script_xsk

    childHrpFor [_,_,_,4,_] hrp
        | hrp == CIP5.root_xsk = pure CIP5.script_xsk

    childHrpFor [_,_,_,_,_] hrp
        | hrp == CIP5.root_xsk = pure CIP5.addr_xsk

    childHrpFor [_,_,_] hrp
        | hrp == CIP5.root_xsk = pure CIP5.acct_xsk

    childHrpFor [2,_] hrp
        | hrp == CIP5.acct_xsk = pure CIP5.stake_xsk
        | hrp == CIP5.acct_xvk = pure CIP5.stake_xvk

    childHrpFor [3,_] hrp
        | hrp == CIP5.acct_xsk = pure CIP5.script_xsk
        | hrp == CIP5.acct_xvk = pure CIP5.script_xvk

    childHrpFor [4,_] hrp
        | hrp == CIP5.acct_xsk = pure CIP5.script_xsk
        | hrp == CIP5.acct_xvk = pure CIP5.script_xvk

    childHrpFor [_,_] hrp
        | hrp == CIP5.root_xsk = pure CIP5.addr_xsk
        | hrp == CIP5.acct_xsk = pure CIP5.addr_xsk
        | hrp == CIP5.acct_xvk = pure CIP5.addr_xvk

    childHrpFor _ hrp
        | hrp == CIP5.root_xsk = fail
            "When deriving child keys from a parent root key, you must \
            \provide either 2, 3 or 5 path segments. Provide 2 (account and \
            \address) if you intend to derive a legacy Byron key. Provide 3 or 5 \
            \(purpose, coin_type, account, role, index) if you're dealing with \
            \anything else."

        | otherwise = fail
            "When deriving child keys from a parent account key, you must \
            \provide exactly two path segments (role & index)."
