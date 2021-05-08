with (import (fetchGit {
  url = https://github.com/dmjio/miso/;
  rev = "0be227bca6e0aec16a69a1c09f9a90624af070c1";
  ref = "master";
}) {}); with pkgs; with haskell.packages // lib; let
  mkBundle = platform: ''
    echo "
      <head>
        <meta charset='utf-8'/>
        <meta name='viewport' content='width=device-width, initial-scale=1'>
        <style>
          `${sass}/bin/sass --style=compressed ${result.src}/src/Views/Dumb/Providing/Root/${
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
            ${result}/bin/${platform}.jsexe/all.js`
        </script>
      </body>
    " > $out/${platform}.html
  '';
  result = ghcjs.callCabal2nix "language-memorizer" ./. {};
in {
  dev = ghc865.callCabal2nix "language-memorizer" ./. {miso = miso-jsaddle;};
  prod = runCommand "prod" {} ''
    mkdir $out
    ${mkBundle "nontouchscreen"}
    ${mkBundle "touchscreen"}
  '';
  inherit pkgs;
}
