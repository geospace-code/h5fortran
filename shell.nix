let
  lock = builtins.fromJSON (builtins.readFile ./flake.lock);
  pkgs = import
    (
      fetchTarball {
        url = "https://github.com/NixOS/nixpkgs/archive/${lock.nodes.nixpkgs.locked.rev}.tar.gz";
        sha256 = lock.nodes.nixpkgs.locked.narHash;
      }
    )
    { };
  hdf5 = pkgs.hdf5.override { fortranSupport = true; };
in
pkgs.mkShell {
  name = "h5fortran";
  packages = [
    pkgs.cmake
    pkgs.ninja
    pkgs.gcc
    pkgs.gfortran
    hdf5
    pkgs.zlib
    pkgs.python3
    pkgs.git
    pkgs.pkg-config
  ];

  shellHook = ''
    echo "h5fortran: GCC (default) + HDF5 + CMake + Ninja"
    echo "Build: cmake -Bbuild -G Ninja --workflow --preset default"
  '';

  HDF5_DIR = "${hdf5.dev}";
}
