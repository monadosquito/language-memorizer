with (import ./default.nix); with pkgs; let mkReloadCmd = platform: ''
  ${haskell.packages.ghc865.ghcid}/bin/ghcid \
    -c 'ghci -fdiagnostics-color=always -ferror-spans -isrc src/Mains/${platform}.hs' \
    -T=:main \
    $(echo "--restart=$(find $PWD -name *.sass)" | sed ':a;N;$!ba;s/\n/\n --restart=/g') \
    --warnings
'';
in dev.env.overrideAttrs (old: {
  buildInputs = [old.buildInputs] ++ [
    (writeScriptBin "reload-nontouchscreen" (mkReloadCmd "Nontouchscreen"))
    (writeScriptBin "reload-touchscreen" (mkReloadCmd "Touchscreen"))
  ];
} // import ./env.nix)
