{ compiler-nix-name ? "ghc923" }:
let
  # Read in the Niv sources
  sources = import ./nix/sources.nix {};
  # If ./nix/sources.nix file is not found run:
  #   niv init
  #   niv add input-output-hk/haskell.nix -n haskellNix

  # Fetch the haskell.nix commit we have pinned with Niv
  haskellNix = import sources.haskellNix { };
  # If haskellNix is not found run:
  #   niv add input-output-hk/haskell.nix -n haskellNix

  # Import nixpkgs and pass the haskell.nix provided nixpkgsArgs
  pkgs = import
    # haskell.nix provides access to the nixpkgs pins which are used by our CI,
    # hence you will be more likely to get cache hits when using these.
    # But you can also just use your own, e.g. '<nixpkgs>'.
    sources.nixpkgs
    # These arguments passed to nixpkgs, include some patches and also
    # the haskell.nix functionality itself as an overlay.
    haskellNix.nixpkgsArgs;
in pkgs.haskell-nix.cabalProject {
    # 'cleanGit' cleans a source directory based on the files known by git
    src = pkgs.haskell-nix.haskellLib.cleanGit {
      src = ./.;
      name = "simhash";
    };
    index-state = "2022-07-30T00:00:00Z";
    index-sha256 = "7527fad4a05f900f88c98a17dd78173a7e225c56ca7f8a7168af1d232d2c72ed";
    plan-sha256 = if compiler-nix-name == "ghc923" then "1x3j427aj4hs5ivjl5wdrlidl7pxl3wv7gfy9afqdrfxhbb4cxzn" else null;
    materialized = if compiler-nix-name == "ghc923" then ./nix/materialized else null;
    # Specify the GHC version to use.
    compiler-nix-name = compiler-nix-name;
  }
