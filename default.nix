with import <nixpkgs> {};

pythonPackages.buildPythonApplication {
  name = "grandpa-0.5";
  src = ./.;

  propagatedBuildInputs = [ pythonPackages.pyserial ];

  buildInputs = with pythonPackages; [ cython gpm ];
}
