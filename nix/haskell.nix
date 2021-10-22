############################################################################
# Builds Haskell packages with Haskell.nix
############################################################################
{ lib
, stdenv
, pkgs
, haskell-nix
, buildPackages
, config ? {}
# GHC attribute name
, compiler ? config.haskellNix.compiler or "ghc8104"
# Enable profiling
, profiling ? config.haskellNix.profiling or false
}:
let

  src = haskell-nix.haskellLib.cleanGit {
      name = "bcc-ledger-specs";
      src = ../.;
  };

  # The bcc-mainnet-mirror used during testing
  bcc-mainnet-mirror = import ./bcc-mainnet-mirror.nix {inherit pkgs;};

  # This creates the Haskell package set.
  # https://The-Blockchain-Company.github.io/haskell.nix/user-guide/projects/
  pkgSet = haskell-nix.cabalProject {
    inherit src;
    compiler-nix-name = compiler;
    modules = [
      {
        packages.cole-spec-chain.configureFlags = [ "--ghc-option=-Werror" ];
        packages.cole-spec-ledger.configureFlags = [ "--ghc-option=-Werror" ];
        packages.delegation.configureFlags = [ "--ghc-option=-Werror" ];
        packages.sophie-spec-non-integral.configureFlags = [ "--ghc-option=-Werror" ];
        packages.sophie-spec-ledger.configureFlags = [ "--ghc-option=-Werror" ];
        packages.bcc-ledger-sophie-ma.configureFlags = [ "--ghc-option=-Werror" ];
        packages.bcc-ledger-sophie-ma-test.configureFlags = [ "--ghc-option=-Werror" ];
        packages.bcc-ledger-sophie-ma-test.components.tests.bcc-ledger-sophie-ma-test.build-tools = [pkgs.cddl pkgs.cbor-diag];
        packages.small-steps.configureFlags = [ "--ghc-option=-Werror" ];
        packages.sophie-spec-ledger-test.components.tests.sophie-spec-ledger-test.build-tools = [pkgs.cddl pkgs.cbor-diag];
        packages.bcc-ledger-aurum-test.components.tests.bcc-ledger-aurum-test.build-tools = [pkgs.cddl pkgs.cbor-diag];
        enableLibraryProfiling = profiling;
        # Disable doctests for now (waiting for https://github.com/The-Blockchain-Company/haskell.nix/pull/427):
        packages.small-steps.components.tests.doctests.buildable = lib.mkForce false;
        packages.small-steps-test.components.tests.doctests.buildable = lib.mkForce false;
        packages.cole-spec-ledger.components.tests.doctests.buildable = lib.mkForce false;

        packages.bcc-ledger-cole = {
          configureFlags = [ "--ghc-option=-Werror" ];
          components = {
            tests.bcc-ledger-cole-test = {
              preCheck = ''
                export BCC_MAINNET_MIRROR="${bcc-mainnet-mirror}/epochs"
                cp ${../cole/ledger/impl/mainnet-genesis.json} ./mainnet-genesis.json
              '';
              build-tools = [ pkgs.makeWrapper ];
              testFlags = [ "--scenario=ContinuousIntegration" ];
            };
          };
        };

      }
    ];
  };
in
  pkgSet
