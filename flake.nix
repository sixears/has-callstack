{
  description = "TypeClass for things that carry around a callstack";

  inputs = {
    nixpkgs.url      = "github:nixos/nixpkgs/be44bf67"; # nixos-22.05 2022-10-15
    build-utils.url  = github:sixears/flake-build-utils/r1.0.0.13;

    more-unicode.url = github:sixears/more-unicode/r0.0.17.12;
    natural.url      = github:sixears/natural/r0.0.1.14;
  };


  outputs = { self, nixpkgs, build-utils
            , more-unicode, natural
            }:
    build-utils.lib.hOutputs self nixpkgs "has-callstack" {
      deps = { inherit more-unicode natural; };
      ghc  = p: p.ghc8107; # for tfmt

      callPackage = { mkDerivation, lib, mapPkg, system
                    , base, base-unicode-symbols, lens, safe, strings, text
                    }:
        let
          pkg = build-utils.lib.flake-def-pkg system;
        in
          mkDerivation {
            pname = "has-callstack";
            version = "1.0.1.19";
            src = ./.;
            libraryHaskellDepends = [
              base base-unicode-symbols lens safe strings text
              (pkg more-unicode) (pkg natural)
            ];
            description = "TypeClass for things that carry around a callstack";
            license = lib.licenses.mit;
          };
    };
}
