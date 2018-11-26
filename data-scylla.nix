{ mkDerivation, aeson, base, containers, data-default, fetchgit
, stdenv, time
}:
mkDerivation {
  pname = "scylla";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/ci-realm/data-scylla.git";
    sha256 = "0yp3wr0g8hfvlbx49w4jrmd01k3a9c7v2bcp9mip8dhbvghywh9i";
    rev = "679e088684fdd8b5e5e0c71f50a24693a8cf5080";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    aeson base containers data-default time
  ];
  homepage = "https://github.com/ci-realm/data-scylla";
  description = "Scylla CI data types";
  license = stdenv.lib.licenses.bsd3;
}
