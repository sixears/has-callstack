{
  description = "TypeClass for things that carry around a callstack";

  inputs = {
    nixpkgs.url      = "github:nixos/nixpkgs/be44bf67"; # nixos-22.05 2022-10-15
    build-utils.url  = "github:sixears/flake-build-utils/r1.0.0.6";

    more-unicode.url = "github:sixears/more-unicode/r0.0.17.5";
    natural.url      = "github:sixears/natural/r0.0.1.5";
  };

  outputs = { self, nixpkgs, build-utils, more-unicode, natural }:
    build-utils.lib.hOutputs self nixpkgs "has-callstack" {
      deps = { inherit more-unicode natural; };
      ghc  = p: p.ghc8107; # for tfmt
    };
}
