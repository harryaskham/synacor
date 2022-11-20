{ mkDerivation, base, binary, extra, hint, lens, lib
, modular-arithmetic, monad-memo, monad-par, monad-par-extras
, plugins, relude, split
}:
mkDerivation {
  pname = "synacor";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base binary extra hint lens modular-arithmetic monad-memo monad-par
    monad-par-extras plugins relude split
  ];
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}
