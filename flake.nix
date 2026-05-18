{
  description = "Haskell course dev shell (GHC + Stack + HLS)";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };

        # Pick a recent compiler toolchain from nixpkgs.
        # If you want to *pin* a specific version for the semester,
        # set it explicitly here (see notes below).
        hsPkgs = pkgs.haskell.packages.ghc984; # change if you prefer (ghc96, ghc910, etc.)
      in
      {
        devShells.default = pkgs.mkShell {
          packages = [
            hsPkgs.ghc
            pkgs.stack
            hsPkgs.haskell-language-server
            pkgs.cabal-install    # handy even if you teach Stack
            pkgs.zlib             # common native dep; keeps build logs calmer
            pkgs.pkg-config       # same
            pkgs.git              # Stack often needs it
          ];

          # Tell Stack to use the Nix-provided GHC instead of downloading its own.
          STACK_IN_NIX_SHELL = "true";
          STACK_SYSTEM_GHC = "true";
          STACK_INSTALL_GHC = "false";

          shellHook = ''
            echo "Haskell dev shell ready."
            echo "GHC: $(ghc --version 2>/dev/null || true)"
            echo "Stack: $(stack --version 2>/dev/null || true)"
            echo "HLS: $(haskell-language-server-wrapper --version 2>/dev/null || true)"
          '';
        };
      });
}
