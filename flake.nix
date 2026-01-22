{
  description = "org-wild-notifier.el development environment";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        # Emacs with required packages for testing
        emacsWithPackages = (pkgs.emacsPackagesFor pkgs.emacs).emacsWithPackages (epkgs: with epkgs; [
          dash
          alert
          async
          package-lint
        ]);

      in {
        devShells.default = pkgs.mkShell {
          buildInputs = [
            emacsWithPackages
            pkgs.just
          ];

          shellHook = ''
            echo "org-wild-notifier.el development environment"
            echo "Run 'just test' to run tests"
          '';
        };

        # Package for running tests
        packages.test = pkgs.runCommand "org-wild-notifier-test" {
          buildInputs = [ emacsWithPackages ];
          src = ./.;
        } ''
          cp -r $src/* .
          chmod -R u+w .
          emacs --batch \
            -L . \
            -L tests \
            -l ert \
            -l tests/org-wild-notifier-tests.el \
            -f ert-run-tests-batch-and-exit
          touch $out
        '';

        # Byte-compile check
        packages.byte-compile = pkgs.runCommand "org-wild-notifier-byte-compile" {
          buildInputs = [ emacsWithPackages ];
          src = ./.;
        } ''
          cp -r $src/* .
          chmod -R u+w .
          emacs --batch -L . -f batch-byte-compile org-wild-notifier.el 2>&1 | tee compile-output.txt
          # Fail if there are errors (not just warnings)
          if grep -q "^>>Error" compile-output.txt; then
            exit 1
          fi
          touch $out
        '';

        # Checkdoc check
        packages.checkdoc = pkgs.runCommand "org-wild-notifier-checkdoc" {
          buildInputs = [ emacsWithPackages ];
          src = ./.;
        } ''
          cp -r $src/* .
          chmod -R u+w .
          emacs --batch -L . -l org-wild-notifier.el \
            --eval "(setq checkdoc-autofix-flag nil)" \
            --eval "(checkdoc-file \"org-wild-notifier.el\")" 2>&1 | tee checkdoc-output.txt
          # Just informational for now - don't fail
          touch $out
        '';

        # Package-lint check
        packages.package-lint = pkgs.runCommand "org-wild-notifier-package-lint" {
          buildInputs = [ emacsWithPackages ];
          src = ./.;
        } ''
          cp -r $src/* .
          chmod -R u+w .
          emacs --batch -L . \
            --eval "(require 'package-lint)" \
            --eval "(setq package-lint-main-file \"org-wild-notifier.el\")" \
            -f package-lint-batch-and-exit org-wild-notifier.el 2>&1 | tee lint-output.txt || true
          # Just informational for now - don't fail
          touch $out
        '';

        # Check for CI - run all checks
        checks.default = self.packages.${system}.test;
      });
}
