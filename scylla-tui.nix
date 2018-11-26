{ mkDerivation, async, base, brick, bytestring, data-default
, microlens, microlens-th, mtl, scylla, scylla-api, stdenv, stm
, template-haskell, text, time, vector, vty, word-wrap
}:
mkDerivation {
  pname = "scylla-tui";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    async base brick bytestring data-default microlens microlens-th mtl
    scylla scylla-api stm template-haskell text time vector vty
    word-wrap
  ];
  homepage = "https://github.com/sorki/scylla-tui";
  description = "Example project";
  license = stdenv.lib.licenses.bsd3;
}
