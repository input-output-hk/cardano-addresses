############################################################################
# Builds Haskell packages with Haskell.nix
############################################################################
{ pkgs
, lib
, stdenv
, haskell-nix
, buildPackages
# GHC attribute name
, compiler
# Enable profiling
, profiling ? false
}:
let

  src = haskell-nix.haskellLib.cleanGit {
      name = "cardano-addresses-src";
      src = ../.;
  };

  # Constraints not in `cabal.project.freeze for cross platform support
  cabalProjectLocal = lib.optionalString stdenv.hostPlatform.isWindows ''
    constraints: Wind32 ==2.6.1.0, mintty ==0.1.2
  '';

  # TODO add flags to packages (like cs-ledger) so we can turn off tests that will
  # not build for windows on a per package bases (rather than using --disable-tests).
  # configureArgs = lib.optionalString stdenv.targetPlatform.isWindows "--disable-tests";
  configureArgs = "";

  # Arguments used as inputs for `cabal configure` (like `configureArgs` and `cabalProjectLocal`)
  # should be passed to this `cabalProject` call as well or `cabal configure` will have to run twice.
  projectPackages = lib.attrNames (haskell-nix.haskellLib.selectProjectPackages
    (haskell-nix.cabalProject { inherit src cabalProjectLocal configureArgs; compiler-nix-name = compiler; }));

  # This creates the Haskell package set.
  # https://input-output-hk.github.io/haskell.nix/user-guide/projects/
  pkgSet = haskell-nix.cabalProject  ({
    # FIXME: without this deprecated attribute, db-converter fails to compile directory with:
  } // {
    inherit src cabalProjectLocal configureArgs;
    compiler-nix-name = compiler;
    modules = [
      # Allow reinstallation of Win32
      ({ pkgs, ... }: lib.mkIf pkgs.stdenv.hostPlatform.isWindows {
        nonReinstallablePkgs =
        [ "rts" "ghc-heap" "ghc-prim" "integer-gmp" "integer-simple" "base"
          "deepseq" "array" "ghc-boot-th" "pretty" "template-haskell"
          # ghcjs custom packages
          "ghcjs-prim" "ghcjs-th"
          "ghc-boot"
          "ghc" "array" "binary" "bytestring" "containers"
          "filepath" "ghc-boot" "ghc-compact" "ghc-prim"
          # "ghci" "haskeline"
          "hpc"
          "mtl" "parsec" "text" "transformers"
          "xhtml"
          # "stm" "terminfo"
        ];
      })
      ({ config, ... }: {
        packages.cardano-addresses.configureFlags = [ "--ghc-option=-Werror" ];
        packages.cardano-addresses-cli.configureFlags = [ "--ghc-option=-Werror" ];

        # This works around an issue with `cardano-addresses-cli.cabal`
        # Haskell.nix does not like `build-tool: cardano-address` as it looks in the
        # cardano-address package instead of the `cardano-addresses-cli`.
        # For some reason `cabal configure` fails if it is changed to:
        # `build-tool-depends: cardano-address-cli:cardano-address
        # Explicitly overriding the `build-tools` allows `build-tool: cardano-address`
        # for now.  A better fix would be to work out why cabal fails when
        # `build-tool-depends` is used.
        packages.cardano-addresses-cli.components.tests.unit.build-tools = pkgs.lib.mkForce [
          config.hsPkgs.buildPackages.hspec-discover.components.exes.hspec-discover
          config.hsPkgs.buildPackages.cardano-addresses-cli.components.exes.cardano-address
        ];
      })

      ({ pkgs, ... }: lib.mkIf (pkgs.stdenv.hostPlatform != pkgs.stdenv.buildPlatform) {
        # Remove hsc2hs build-tool dependencies (suitable version will be available as part of the ghc derivation)
        packages.Win32.components.library.build-tools = lib.mkForce [];
        packages.terminal-size.components.library.build-tools = lib.mkForce [];
        packages.network.components.library.build-tools = lib.mkForce [];
      })

      ({ pkgs, ... }: lib.mkIf (pkgs.stdenv.hostPlatform != pkgs.stdenv.buildPlatform) {
        # Make sure we use a buildPackages version of happy
        packages.pretty-show.components.library.build-tools = [ buildPackages.haskell-nix.haskellPackages.happy ];

        # Remove hsc2hs build-tool dependencies (suitable version will be available as part of the ghc derivation)
        packages.Win32.components.library.build-tools = lib.mkForce [];
        packages.terminal-size.components.library.build-tools = lib.mkForce [];
        packages.network.components.library.build-tools = lib.mkForce [];

        # Disable cabal-doctest tests by turning off custom setups
        packages.comonad.package.buildType = lib.mkForce "Simple";
        packages.distributive.package.buildType = lib.mkForce "Simple";
        packages.lens.package.buildType = lib.mkForce "Simple";
        packages.nonempty-vector.package.buildType = lib.mkForce "Simple";
        packages.semigroupoids.package.buildType = lib.mkForce "Simple";
      })

      ({pkgs, config, ... }: lib.mkIf pkgs.stdenv.hostPlatform.isGhcjs {
        packages.digest.components.library.libs = lib.mkForce [ pkgs.buildPackages.zlib ];
        packages.cardano-addresses-cli.components.library.build-tools = [ pkgs.buildPackages.buildPackages.gitMinimal ];
        # Run the script to build the C sources from cryptonite and cardano-crypto
        # and place the result in jsbits/cardano-crypto.js
        packages.cardano-addresses-cli.components.exes.cardano-address.preConfigure = ''
          script=$(mktemp -d)
          cp -r ${../script}/* $script
          ln -s ${pkgs.srcOnly {name = "cryptonite-src"; src = config.packages.cryptonite.src;}}/cbits $script/cryptonite
          ln -s ${pkgs.srcOnly {name = "cardano-crypto-src"; src = config.packages.cardano-crypto.src;}}/cbits $script/cardano-crypto
          patchShebangs $script/build.sh
          (cd $script && PATH=${pkgs.buildPackages.emscripten}/bin:${pkgs.buildPackages.buildPackages.closurecompiler}/bin:$PATH ./build.sh || echo DOH)
          mkdir -p jsbits
          cp $script/cardano-crypto.js jsbits/cardano-crypto.js
        '';
      })
    ];
  });
in
  pkgSet
