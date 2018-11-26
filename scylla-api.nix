{ mkDerivation, aeson, ansi-wl-pprint, base, binary, bytestring
, config-ini, containers, data-default, directory, fetchgit
, filepath, mtl, network, optparse-applicative
, pretty-relative-time, scylla, split, stdenv, stm, text, time
, websockets, wuss
}:
mkDerivation {
  pname = "scylla-api";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/ci-realm/scylla-api.git";
    sha256 = "11i26s0srhq06lxyr5cfv7f459k358zb6nvd1z3qyx510jrygfnl";
    rev = "4f21382327e54e24c37900ff12c0e3651cb519a3";
    fetchSubmodules = true;
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson ansi-wl-pprint base binary bytestring config-ini containers
    data-default directory filepath mtl network pretty-relative-time
    scylla split stm text time websockets wuss
  ];
  executableHaskellDepends = [
    aeson base bytestring containers data-default mtl
    optparse-applicative scylla stm text
  ];
  homepage = "https://github.com/ci-realm/scylla-api";
  description = "API for Scylla CI";
  license = stdenv.lib.licenses.bsd3;
}
