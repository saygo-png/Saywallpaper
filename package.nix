{
  niceHaskell,
  ...
}:
niceHaskell.mkPackage {
  flags = niceHaskell.mkFlags {doCheck = false;};
  packageRoot = ./.;
  cabalName = "saybar";
  compiler = "ghc912";
}
