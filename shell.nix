with (import ./default.nix); with pkgs; let
  env = import ./env.nix;
  mkReloadClientCmd = mkReloadCmd "client" true;
  mkReloadCmd = appType: isMultiPlatform: platform: ''
    ${haskell.packages.ghc865.ghcid}/bin/ghcid \
      -c 'ghci -fdiagnostics-color=always -ferror-spans -i${appType}/src \
        ${appType}/src/${if isMultiPlatform then "Mains/" else ""}${platform}.hs' \
      -T=:main \
      $(echo "--restart=$(find $PWD -name *.sass)" | sed ':a;N;$!ba;s/\n/\n --restart=/g') \
      --warnings
  '';
in {
  clientDev = clientDev.env.overrideAttrs (old: {buildInputs = [old.buildInputs] ++ [
    (writeScriptBin "reload-nontouchscreen" (mkReloadClientCmd "Nontouchscreen"))
    (writeScriptBin "reload-touchscreen" (mkReloadClientCmd "Touchscreen"))
  ];} // env.client.dev);
  server = server.env.overrideAttrs (old: {buildInputs = [old.buildInputs] ++ [
    (writeScriptBin "reload" (mkReloadCmd "server" false "Main"))
  ];} // env.server.dev);
}
