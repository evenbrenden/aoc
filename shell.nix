with import <nixpkgs> {};

pkgs.mkShell {
  buildInputs = with pkgs; [
    ghcid
    (haskellPackages.ghcWithPackages (p: [p.trifecta]))
  ];
}
