{
  niceHaskell,
  pkgs,
  ...
}:
niceHaskell.mkPackage {
  flags = niceHaskell.mkFlags {doCheck = false;};
  packageRoot = ./.;
  cabalName = "saybar";
  compiler = "ghc912";
  developPackageArgs.overrides = _hsFinal: hsPrev: {
    nanovg = pkgs.haskell.lib.markUnbroken (
      hsPrev.nanovg.override {
        containers = hsPrev.callHackage "containers" "0.6.8" {};
      }
    );
  };
}
