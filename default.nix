with (import (fetchGit {
  url = https://github.com/dmjio/miso/;
  rev = "0be227bca6e0aec16a69a1c09f9a90624af070c1";
  ref = "master";
}) {}); with pkgs; with haskell.packages // lib; with ghc865; let
  clientResult = ghcjs.callCabal2nix "client" ./client {
    common = ghcjs.callCabal2nix "common" ./common {};
  };
  mkClientBundle = platform: ''
    echo "
      <head>
        <meta charset='utf-8'/>
        <meta name='viewport' content='width=device-width, initial-scale=1'>
        <style>
          `${sass}/bin/sass --style=compressed ${clientResult.src}/src/Views/Dumb/Providing/Root/${
              concatImapStrings
                (ix: char: if ix == 1 then (toUpper char) else char)
                (stringToCharacters platform)
            }.sass`
        </style>
        <title>language-memorizer</title>
      </head>
      <body>
        <script>
          `${closurecompiler}/bin/closure-compiler --jscomp_off='*' -O=ADVANCED \
            ${clientResult}/bin/${platform}.jsexe/all.js`
        </script>
      </body>
    " > $out/${platform}.html
  '';
in rec {
  clientDev = callCabal2nix "client" ./client {miso = miso-jsaddle; inherit common;};
  common = callCabal2nix "common" ./common {};
  prod = runCommand "prod" {} ''
    mkdir $out
    ${mkClientBundle "nontouchscreen"}
    ${mkClientBundle "touchscreen"}
    cp ${server}/bin/server $out/server
  '';
  server = callCabal2nix "server" ./server {inherit common;};
  inherit pkgs;
}
