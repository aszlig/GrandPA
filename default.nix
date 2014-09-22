let
  pkgs = import <nixpkgs> {};

  inherit (pkgs) SDL2;
  inherit (pkgs.haskellPackages) cabal c2hs cereal stm netwire;

  libftdi1 = with pkgs; stdenv.mkDerivation rec {
    name = "libftdi1-${version}";
    version = "1.1";

    src = fetchurl {
      url = "http://www.intra2net.com/en/developer/libftdi/download/"
          + "${name}.tar.bz2";
      sha256 = "088yh8pxd6q53ssqndydcw1dkq51cjqyahc03lm6iip22cdazcf0";
    };

    cmakeFlags = [ "-DLIB_SUFFIX=" ];

    nativeBuildInputs = [ cmake pkgconfig swig ];
    propagatedBuildInputs = [ libusb1 ];
    buildInputs = [ boost ];

    meta = {
      description = "A library to talk to FTDI chips using libusb";
      homepage = http://www.intra2net.com/en/developer/libftdi/;
      license = stdenv.lib.licenses.gpl2Plus;
    };
  };

  sdl2 = cabal.mkDerivation (self: {
    pname = "sdl2";
    version = "1.1.0";
    sha256 = "1ppxskh810nbziszlkdmk38x74lspsrqm1kpyiir1xj2a7122fkv";
    extraLibraries = [ SDL2 ];
    pkgconfigDepends = [ SDL2 ];
    meta = {
      description = "Bindings to SDL2";
      license = self.stdenv.lib.licenses.bsd3;
      platforms = self.ghc.meta.platforms;
    };
  });

in cabal.mkDerivation (self: {
  pname = "grandpa";
  version = "0.1.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  pkgconfigDepends = [ libftdi1 ];
  buildDepends = [ cereal stm netwire sdl2 ];
  buildTools = [ c2hs ];
  meta = {
    homepage = "https://github.com/rockfabrik/grandpa";
    description = "The GrandPA LedBar lighting controller";
    license = self.stdenv.lib.licenses.gpl3;
    platforms = self.ghc.meta.platforms;
  };
})
