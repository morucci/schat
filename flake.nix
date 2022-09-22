{
  description = "schat";
  nixConfig.bash-prompt = "[nix(schat)] ";

  inputs = {
    hspkgs.url =
      "github:podenv/hspkgs/70ebfaa3e643ce20fe8ff97c17693ceb4bfaa30d";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, hspkgs, flake-utils }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        packageName = "schat";
        pkgs = hspkgs.pkgs;
        haskellPackages = pkgs.hspkgs;
        myPackage = haskellPackages.callCabal2nix packageName self { };

      in {
        defaultExe = pkgs.haskell.lib.justStaticExecutables myPackage;
        defaultPackage = myPackage;

        devShell = haskellPackages.shellFor {
          packages = p: [ myPackage ];

          buildInputs = [
            pkgs.ghcid
            pkgs.ormolu
            pkgs.cabal-install
            pkgs.hlint
            pkgs.haskell-language-server
          ];
        };
      });
}
