{ mkDerivation, base, fetchgit, genvalidity-hspec, genvalidity-time
, hpack, hspec, QuickCheck, stdenv, time, validity, validity-time
}:
mkDerivation {
  pname = "pretty-relative-time";
  version = "0.0.0.0";
  src = fetchgit {
    url = "https://github.com/NorfairKing/pretty-relative-time";
    sha256 = "0g7i1nb52l41ka99z4s8ylm9w7jxmihr5s7rcp6kb7splmg99i6h";
    rev = "85ee8a577fb576e2dd7643bf248ff8fbbe9598ec";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [ base time validity validity-time ];
  libraryToolDepends = [ hpack ];
  testHaskellDepends = [
    base genvalidity-hspec genvalidity-time hspec QuickCheck time
    validity validity-time
  ];
  preConfigure = "hpack";
  homepage = "https://github.com/NorfairKing/pretty-relative-time#readme";
  description = "Pretty relative time";
  license = stdenv.lib.licenses.mit;
}
