{
  description = "A flake for developing and using clash-circuit-context";
  inputs = {
    # The same clash-compiler commit as cabal.project, so that Nix and cabal
    # build against the same clash-prelude.
    clash-compiler.url = "github:clash-lang/clash-compiler/e16599387a68d6361134b4fe7e63bf4ad3ae8408";
    nixpkgs.follows = "clash-compiler/nixpkgs";
    flake-utils.follows = "clash-compiler/flake-utils";

    # Tool used to manage the changelog, see 'changelog/README.md'. Pinned,
    # because the flake of later revisions misses the `colorama` dependency.
    qlog = {
      url = "github:QBayLogic/qlog/3bea2429e555e969984fbf897216cee74df2f73d";
      inputs.flake-utils.follows = "flake-utils";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
  outputs = { self, flake-utils, nixpkgs, clash-compiler, qlog, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        # The package to expose as 'default'
        default-package = "clash-circuit-context";
        # The 'default' version of ghc to use
        default-version = clash-compiler.ghcVersion.${system};
        # A list of all ghc versions this package supports
        supported-versions = clash-compiler.supportedGhcVersions.${system};

        generic-pkgs = import nixpkgs {
          inherit system;
        };

        qlog-pkg = qlog.packages.${system}.default;

        all-overlays = builtins.listToAttrs (builtins.map (compiler-version:
          let
            overlay = final: prev: {
              # The clash package set ships clash-shockwaves 1.0.0, which wants
              # clash-prelude <1.10, but clash-circuit-context requires >=1.1.1.
              # Pull 1.1.3 from Hackage: the release clash-circuit-context.cabal
              # verifies, and the first that builds on GHC 9.6.
              clash-shockwaves = prev.callHackageDirect {
                pkg = "clash-shockwaves";
                ver = "1.1.3";
                sha256 = "sha256-8wOfkgwKYWgumRnCzaJuG+YYQGuY7FTtr308H/f8MVA=";
              } {};

              clash-circuit-context = prev.developPackage {
                root = ./.;
                overrides = _: _: final;
                # libfst links the C zlib (extra-libraries: z). cabal2nix names
                # that argument zlib, which this package set resolves to the
                # Haskell binding, so give the C library explicitly.
                modifier = drv:
                  generic-pkgs.haskell.lib.compose.addExtraLibrary generic-pkgs.zlib drv;
              };
            };
          in
            { name = compiler-version; value = overlay; }
          ) supported-versions);

        all-hs-pkgs = builtins.mapAttrs (compiler-version: overlay:
          let
            pkgs = generic-pkgs.extend clash-compiler.overlays.${compiler-version};
            clash-pkgs = pkgs."clashPackages-${compiler-version}";

            hs-pkgs = clash-pkgs.extend overlay;
          in
            hs-pkgs
          ) all-overlays;

        minimal-shell = hs-pkgs: hs-pkgs.shellFor {
          packages = p: [
            p.clash-circuit-context
          ];

          # https://discourse.nixos.org/t/non-interactive-bash-errors-from-flake-nix-mkshell/33310
          buildInputs = [
            generic-pkgs.bashInteractive
          ];

          nativeBuildInputs = [
            hs-pkgs.cabal-install
            hs-pkgs.cabal-plan
            hs-pkgs.fourmolu
            qlog-pkg
          ];
        };

        all-shells = nixpkgs.lib.attrsets.concatMapAttrs (name: hs-pkgs: {
            # The difference between the `-minimal` and `-full` is the addition of HLS in the full version
            # This is because HLS is slow to compile and not everyone uses it
            # We default to using the `-minimal` version when `nix develop`ing
            "${name}-minimal" = minimal-shell hs-pkgs;
            "${name}-full" = (minimal-shell hs-pkgs).overrideAttrs (fAttr: pAttr: {
              nativeBuildInputs = pAttr.nativeBuildInputs ++ [
                hs-pkgs.haskell-language-server
              ];
            });
          }) all-hs-pkgs;

        all-packages = builtins.mapAttrs (_: hs-pkgs:
          {
            clash-circuit-context = hs-pkgs.clash-circuit-context;

            default = hs-pkgs.${default-package};
          }) all-hs-pkgs;
      in
      {
        # Expose the overlay of each supported version which adds clash-circuit-context
        # The base of the overlay is clash-pkgs
        overlays = all-overlays // { default = all-overlays.${default-version}; };

        # A devShell for each supported version
        #
        # These can be invoked using `nix develop .#ghc9103-minimal`
        devShells = all-shells // { default = all-shells."${default-version}-minimal"; };

        # The default directly refers to the default package of the default ghc version of this flake
        # All other entries aren't packages, they're a set of packages for each supported ghc version
        packages = all-packages // { default = all-packages.${default-version}.${default-package}; };

        # `nix run .#qlog`, without the Haskell development shell
        apps.qlog = {
          type = "app";
          program = "${qlog-pkg}/bin/qlog";
        };
      });
}
