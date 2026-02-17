{pkgs, ...}: {
  languages.ocaml.enable = true;

  packages = with pkgs.ocamlPackages; [
    base
    ppx_deriving
  ];
}
