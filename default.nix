{ mkDerivation, base, brick, containers, dhall, optparse-generic
, stdenv, text, transformers, trifecta, vty
}:
mkDerivation {
  pname = "dhall-edit";
  version = "1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base brick containers dhall text transformers vty
  ];
  executableHaskellDepends = [
    base brick dhall optparse-generic text transformers trifecta vty
  ];
  description = "Autogenerate a curses editor for a Dhall configuration file";
  license = stdenv.lib.licenses.bsd3;
}
