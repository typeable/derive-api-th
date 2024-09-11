{ reflex-platform ? ((import <nixpkgs> {}).fetchFromGitHub {
    owner = "reflex-frp";
    repo = "reflex-platform";
    rev = "a4664e3b99f1b89a77ac759501b6bd85e18eac95";
    sha256 = "1pjzjhj966rs16f4b80ir9v8d7g7q2aa0i0zs30fny3x0xk1b4ah";
    })
}:
(import reflex-platform { config.allowBroken = true; }).project ({ pkgs, ... }:{
  useWarp = true;
  withHoogle = false;

  packages = {
    derive-api-th = ../derive-api-th;
  };

  tools = _: [
    (pkgs.haskell.lib.dontCheck pkgs.haskellPackages.cabal-cargs)
    pkgs.inotify-tools
  ];

  shellToolOverrides = ghc: super: {
    closure-compiler = null;
    haskell-ide-engine = null;
    hdevtools = null;
    hlint = null;
    stylish-haskell = null;
    hoogle = null;
  };

  shells = {
    ghc   = ["derive-api-th"];
    ghcjs = ["derive-api-th"];
  };
})
