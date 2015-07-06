with import <nixpkgs> {};

buildPythonPackage {
  name = "grandpa-0.5";
  src = ./.;

  propagatedBuildInputs = with pythonPackages; [
    python.modules.curses
    python.modules.bsddb
    pyserial
  ];

  buildInputs = [ cython gpm ];
}
