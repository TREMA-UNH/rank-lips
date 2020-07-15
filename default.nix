{ nixpkgs ? (import ./simplir/nixpkgs.nix {}) }:

let
  inherit (nixpkgs.haskell.lib) dontCheck doJailbreak;
  inherit (nixpkgs.stdenv) lib;

  all-cabal-hashes =
    let
      rev = "30a0c2f2c25056349249cda6aec4428c2229e3b8";
    in builtins.fetchurl {
      url    = "https://github.com/commercialhaskell/all-cabal-hashes/archive/${rev}.tar.gz";
      sha256 = "1a3zvq1yr4wm335y8zndn08d3yjjg51kk6p8lx11jpn1j28si0k8";
    };

  localDir = nixpkgs.nix-gitignore.gitignoreSourcePure [ ./.gitignore ];

  simplirNix = import ./simplir { inherit nixpkgs; };

  haskellOverrides = self: super:
    let
      rankLipsPackages = {
        rank-lips            = self.callCabal2nix "rank-lips" (localDir ./.) {};
        graph-algorithms     = self.callCabal2nix "graph-algorithms" (localDir ./simplir/graph-algorithms) {};

        hpc-coveralls = doJailbreak (self.callCabal2nix "hpc-coveralls" (nixpkgs.fetchFromGitHub {
          owner = "bgamari";
          repo = "hpc-coveralls";
          rev = "a2d500316fecb8ee49c034e2781862c8606b96af";
          sha256 = "17d3ljibsdsxbsqrdjx6rn0ww8ck0lycp2pwfh71ilvwbm5wlbyb";
        }) {});

        frisby = self.callHackage "frisby" "0.2.4" {};
        http-media = doJailbreak (self.callHackage "http-media" "0.8.0.0" {});
      };
    in rankLipsPackages // { inherit rankLipsPackages; };

  haskellPackages = nixpkgs.haskell.packages.ghc883.override {
    overrides = lib.composeExtensions simplirNix.haskellOverrides haskellOverrides;
    inherit all-cabal-hashes;
  };
in {
  pkgs = nixpkgs;
  inherit haskellPackages haskellOverrides;
  inherit (simplirNix) simplirPackages trec-eval;
  env = haskellPackages.ghcWithHoogle (pkgs:
    builtins.attrValues haskellPackages.simplirPackages
    ++ (with pkgs; [ zlib ]));
 binaries = nixpkgs.symlinkJoin {
    name = "rank-lips-binaries";
    paths = builtins.attrValues haskellPackages.rankLipsPackages ++ builtins.attrValues haskellPackages.simplirPackages;
  };
}

