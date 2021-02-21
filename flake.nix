{
  description = "DCPU-16 Implementation in Haskell";
  inputs = {
    flake-utils.url = "github:numtide/flake-utils/master";
    nixpkgs.url = "github:nixos/nixpkgs/master";
  };

  outputs = { self, nixpkgs, flake-utils }@inputs:
    with flake-utils.lib;
    eachSystem [ "x86_64-linux" ] (system:
      let overlays = [
            (final: prev:
              with final;
              with final.lib;
              with haskellPackages;
              with haskell.lib;
              let withVersion = d: overrideCabal d (o: {
                    version = o.version + "-${substring 0 8 self.lastModifiedDate}.${self.shortRev or "dirty"}";
                  });
              in rec {
                cpu16 = withVersion (callCabal2nix "cpu16" ./cpu16 {});  
              })
          ];
          pkgs = import nixpkgs { inherit system overlays; };
      in rec {
        packages = flattenTree rec {
          cpu16 = pkgs.cpu16;
        };
      });
}
