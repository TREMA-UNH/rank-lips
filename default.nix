{ nixpkgs ? (import ./trec-car-tools-haskell/simplir/nixpkgs.nix {}) }:

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

  simplirNix = import ./trec-car-tools-haskell/simplir { inherit nixpkgs; };

  haskellOverrides = self: super:
    let
      trecCarPackages = {
        trec-car-types       = dontCheck (self.callCabal2nix "trec-car-types" (localDir ./trec-car-tools-haskell/trec-car-types) {});
        trec-car-tools       = dontCheck (self.callCabal2nix "trec-car-tools" (localDir ./trec-car-tools-haskell/trec-car-tools) {});
        mediawiki-parser     = self.callCabal2nix "mediawiki-parser" (localDir ./mediawiki-parser) {};
        mediawiki-import     = self.callCabal2nix "mediawiki-import" (localDir ./mediawiki-import) {};
        mediawiki-convert    = self.callCabal2nix "mediawiki-convert" (localDir ./mediawiki-convert) {};
        car-baselines        = self.callCabal2nix "car-baselines" (localDir ./car-baselines) {};
        filter-duplicates    = self.callCabal2nix "filter-duplicates" (localDir ./filter-duplicates) {};
        assessment-interface = self.callCabal2nix "trec-car-annotation-interface" (localDir ./assessment-interface) {};
        assessment-eval      = self.callCabal2nix "assessment-eval" (localDir ./assessment-eval) {};
        annotate-server      = self.callCabal2nix "annotate-server" (localDir ./assessment-interface/annotation/server) {};
        graph-algorithms     = self.callCabal2nix "graph-algorithms" (localDir ./trec-car-tools-haskell/simplir/graph-algorithms) {};
        db-export            = self.callCabal2nix "db-export" (localDir ./db-export) {};
        evalmetrics          = self.callCabal2nix "evalmetrics" (localDir ./evalmetrics) {};
        wordnet-export       = nixpkgs.callPackage (import ./wordnet-export) { haskellPackages = self; };
        multilang-car        = self.callCabal2nix "multilang-car" (localDir ./multilang-car) {};
        tqa-import           = self.callCabal2nix "tqa-import" (localDir ./tqa-import) {};
        trec-news            = self.callCabal2nix "trec-news" (localDir ./trec-news) {};
        epfl-section-recommendation = self.callCabal2nix "epfl-section-recommendation" (localDir ./epfl-section-recommendation) {};
        dbpedia-entity-import= self.callCabal2nix "dbpedia-entity-import" (localDir ./dbpedia-entity-import) {};
        tag-me               = self.callCabal2nix "tag-me" (localDir ./tag-me) {};
        miso-types           = self.callCabal2nix "miso-types" (localDir ./miso-types) {};
        eal-tools            = self.callCabal2nix "eal-tools" (localDir ./eal-tools) {};
        rank-lips            = self.callCabal2nix "rank-lips" (localDir ./rank-lips) {};

        intset = self.callCabal2nix "intset" ./vendor/intset {};
        hpc-coveralls = doJailbreak (self.callCabal2nix "hpc-coveralls" (nixpkgs.fetchFromGitHub {
          owner = "bgamari";
          repo = "hpc-coveralls";
          rev = "a2d500316fecb8ee49c034e2781862c8606b96af";
          sha256 = "17d3ljibsdsxbsqrdjx6rn0ww8ck0lycp2pwfh71ilvwbm5wlbyb";
        }) {});

        frisby = self.callHackage "frisby" "0.2.4" {};
        http-media = doJailbreak (self.callHackage "http-media" "0.8.0.0" {});
      };
    in trecCarPackages // { inherit trecCarPackages; };

  haskellPackages = nixpkgs.haskell.packages.ghc883.override {
    overrides = lib.composeExtensions simplirNix.haskellOverrides haskellOverrides;
    inherit all-cabal-hashes;
  };
in {
  pkgs = nixpkgs;
  inherit (haskellPackages.wordnet-export.passthru) ukb;
  inherit haskellPackages haskellOverrides;
  inherit (haskellPackages) trecCarPackages;
  inherit (simplirNix) simplirPackages trec-eval;
  env = haskellPackages.ghcWithHoogle (pkgs: builtins.attrValues haskellPackages.trecCarPackages ++ builtins.attrValues haskellPackages.simplirPackages);
  binaries = nixpkgs.symlinkJoin {
    name = "trec-car-binaries";
    paths = builtins.attrValues haskellPackages.trecCarPackages ++ builtins.attrValues haskellPackages.simplirPackages;
  };
}

