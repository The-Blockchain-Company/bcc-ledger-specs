{ system ? builtins.currentSystem
, crossSystem ? null
# allows to cutomize haskellNix (ghc and profiling, see ./nix/haskell.nix)
, config ? {}
# allows to override dependencies of the project without modifications,
# eg. to test build against local checkout of tbco-nix:
# nix build -f default.nix bcc-node --arg sourcesOverride '{
#   tbco-nix = ../tbco-nix;
# }'
, sourcesOverride ? {}
# pinned version of nixpkgs augmented with overlays (tbco-nix and our packages).
, pkgs ? import ./nix { inherit system crossSystem config sourcesOverride; }
, gitrev ? pkgs.tbcoNix.commitIdFromGitRepoOrZero ./.git
}:
with pkgs; with commonLib;

let
  haskellPackages = recRecurseIntoAttrs
    # we are only interested in listing the project packages:
    (selectProjectPackages bccLedgerSpecsHaskellPackages);

  self = {
    inherit haskellPackages check-hydra;

    # `tests` are the test suites which have been built.
    tests = collectComponents' "tests" haskellPackages;
    # `benchmarks` (only built, not run).
    benchmarks = collectComponents' "benchmarks" haskellPackages;

    libs = collectComponents' "library" haskellPackages;

    exes = collectComponents' "exes" haskellPackages;

    checks = recurseIntoAttrs {
      # `checks.tests` collect results of executing the tests:
      tests = collectChecks haskellPackages;
    };

    shell = import ./shell.nix {
      inherit pkgs;
      withHoogle = true;
    };

    roots = bccLedgerSpecsHaskellPackages.roots;

    #
    # PDF builds of LaTeX documentation.
    #
    # To download the latest PDF build from Hydra, use this link:
    #   https://hydra.tbco.io/job/Bcc/bcc-ledger-specs/specs.NAME/latest/download/1/NAME.pdf
    #
    # To get a shell where you can run pdflatex to build it yourself, use:
    #   nix-shell default.nix -A specs.NAME
    #
    # To build all specs locally with Nix:
    #  nix-build -A specs -o spec
    #
    specs = recurseIntoAttrs {
      cole-ledger = pkgs.callPackage ./cole/ledger/formal-spec/default.nix {};
      cole-chain = pkgs.callPackage ./cole/chain/formal-spec/default.nix {};
      small-step-semantics = pkgs.callPackage ./semantics/formal-spec/default.nix {};
      sophie-ledger = pkgs.callPackage ./sophie/chain-and-ledger/formal-spec/default.nix {};
      pool-ranking = pkgs.callPackage ./sophie/pool-ranking/default.nix {};
      sophie-ma = pkgs.callPackage ./sophie-ma/formal-spec/default.nix {};
      aurum-ledger = pkgs.callPackage ./aurum/formal-spec/default.nix {};
      delegation-design = pkgs.callPackage ./sophie/design-spec/default.nix {};
      non-integer-calculations = pkgs.callPackage ./sophie/chain-and-ledger/dependencies/non-integer/doc/default.nix {};
      blocks-cddl = pkgs.callPackage ./cole/cddl-spec/default.nix {};
    };

    doc = {
      site =
        let
          sphinx-markdown-tables = pkgs.python3Packages.callPackage ./nix/python/sphinx-markdown-tables.nix {};
          sphinxemoji = pkgs.python3Packages.callPackage ./nix/python/sphinxemoji.nix {};
        in pkgs.callPackage ./doc { inherit sphinx-markdown-tables sphinxemoji; pythonPackages = pkgs.python3Packages; };
    };
  };

in
  self
