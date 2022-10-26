{
  description = "TypeClass for things that carry around a callstack";

  inputs = {
    nixpkgs.url      = "github:nixos/nixpkgs/be44bf67"; # nixos-22.05 2022-10-15
    build-utils.url  = "github:sixears/flake-build-utils/r0.0.0.0";
    more-unicode.url = "github:sixears/more-unicode/r0.0.17.0";
    natural.url      = "github:sixears/natural/r0.0.1.0";
  };

  outputs = { self, nixpkgs, flake-utils, build-utils, more-unicode, natural }:
    build-utils.lib.hOutputs self nixpkgs "has-callstack" {
      inherit more-unicode natural;
    };
}
