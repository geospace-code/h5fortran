{
  description = "h5fortran development shells";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.11";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs {
          inherit system;
          config = {
            allowUnfree = true;
          };
        };

        common-build-tools = with pkgs; [
          cmake
          ninja
          pkg-config
          git
        ];

        mkGccShell =
          {
            name,
            gcc-pkg,
            gfortran-pkg,
            packages ? [ ],
            shellHookExtra ? "",
          }:
          pkgs.mkShell {
            inherit name;
            packages =
              common-build-tools
              ++ [
                gcc-pkg
                gfortran-pkg
                pkgs.hdf5
                pkgs.zlib
                pkgs.python3
              ]
              ++ packages;

            shellHook = ''
              export CC=${gcc-pkg}/bin/gcc
              export FC=${gfortran-pkg}/bin/gfortran
              export CXX=${gcc-pkg}/bin/g++
              echo "h5fortran: ${name}"
              echo "CC: $CC  FC: $FC  CXX: $CXX"
              echo "HDF5: ${pkgs.hdf5.dev}"
              ${shellHookExtra}
            '';

            HDF5_DIR = "${pkgs.hdf5.dev}";
          };
      in
      {
        devShells.default = mkGccShell {
          name = "h5fortran-gcc-default";
          gcc-pkg = pkgs.gcc14;
          gfortran-pkg = pkgs.gfortran14;
          shellHookExtra = ''echo "Build: cmake -Bbuild -G Ninja --workflow --preset default"'';
        };

        devShells.gcc-old = mkGccShell {
          name = "h5fortran-gcc-11";
          gcc-pkg = pkgs.gcc11;
          gfortran-pkg = pkgs.gfortran11;
          shellHookExtra = ''echo "Build: cmake -Bbuild -G Ninja --workflow --preset default"'';
        };

        devShells.gcc-new = mkGccShell {
          name = "h5fortran-gcc-14";
          gcc-pkg = pkgs.gcc14;
          gfortran-pkg = pkgs.gfortran14;
          shellHookExtra = ''echo "Build: cmake -Bbuild -G Ninja --workflow --preset default"'';
        };

        devShells.flang = pkgs.mkShell {
          name = "h5fortran-flang";
          packages =
            common-build-tools
            ++ [
              pkgs.llvmPackages.clang
              pkgs.llvmPackages.flang
              pkgs.llvmPackages.llvm
              pkgs.llvmPackages.libomp
              pkgs.hdf5
              pkgs.zlib
            ];

          shellHook = ''
            export CC=${pkgs.llvmPackages.clang}/bin/clang
            export CXX=${pkgs.llvmPackages.clang}/bin/clang++
            export FC=${pkgs.llvmPackages.flang}/bin/flang-new
            echo "h5fortran: LLVM/Flang + HDF5 + CMake + Ninja"
            echo "HDF5: ${pkgs.hdf5.dev}"
          '';

          HDF5_DIR = "${pkgs.hdf5.dev}";
        };

        devShells.fpm = pkgs.mkShell {
          name = "h5fortran-fpm";
          packages =
            common-build-tools
            ++ [
              pkgs.gcc14
              pkgs.gfortran14
              pkgs.hdf5
              pkgs.zlib
              pkgs.fpm
              pkgs.python3
            ];

          shellHook = ''
            export CC=${pkgs.gcc14}/bin/gcc
            export FC=${pkgs.gfortran14}/bin/gfortran
            export CXX=${pkgs.gcc14}/bin/g++
            export FPM_FFLAGS="-I${pkgs.hdf5.dev}/include"
            export FPM_LDFLAGS="-L${pkgs.hdf5.lib}/lib"
            echo "h5fortran: fpm + GCC + HDF5"
            echo "Build: fpm build"
          '';
        };

        devShells.testing = mkGccShell {
          name = "h5fortran-testing";
          gcc-pkg = pkgs.gcc14;
          gfortran-pkg = pkgs.gfortran14;
          packages = with pkgs; [
            valgrind
            gcovr
          ];
          shellHookExtra = ''
            echo "Tools: valgrind, gcovr, python (h5py/numpy)"
            echo "Build: cmake -Bbuild -G Ninja --workflow --preset default"
          '';
        };

        devShells.cmake-old = pkgs.mkShell {
          name = "h5fortran-cmake-old";
          packages = with pkgs; [
            cmake_3_24
            ninja
            pkg-config
            gcc14
            gfortran14
            hdf5
            zlib
            git
          ];

          shellHook = ''
            export CC=${pkgs.gcc14}/bin/gcc
            export FC=${pkgs.gfortran14}/bin/gfortran
            export CXX=${pkgs.gcc14}/bin/g++
            echo "h5fortran: CMake 3.24 (old) + GCC 14 + HDF5 + Ninja"
            echo "HDF5: ${pkgs.hdf5.dev}"
          '';

          HDF5_DIR = "${pkgs.hdf5.dev}";
        };

        devShells.oneapi = pkgs.mkShell {
          name = "h5fortran-oneapi";
          packages = with pkgs;
            common-build-tools
            ++ [
              pkgs.intel-oneapi.hpc
              pkgs.hdf5-fortran
              pkgs.zlib
              pkgs.python3
              pkgs.stdenv.cc
            ];

          shellHook = ''
            # Source Intel oneAPI environment
            if [ -d /opt/intel/oneapi ]; then
              source /opt/intel/oneapi/setvars.sh >/dev/null 2>&1
            fi

            # Create icx wrapper with proper linker paths
            _NIX_GLIBC_LIB=$(dirname $(gcc -print-file-name=Scrt1.o))
            _NIX_GCC_CRT=$(dirname $(gcc -print-file-name=crtbeginS.o))
            _NIX_GCC_SLIB=$(dirname $(gcc -print-file-name=libgcc_s.so))
            _ICX_WRAPPER=$(mktemp /tmp/icx-wrapper-XXXXXX)
            cat > "$_ICX_WRAPPER" << EOF
#!/usr/bin/env bash
exec ${pkgs.intel-oneapi.hpc}/bin/icx -L$_NIX_GLIBC_LIB -L$_NIX_GCC_CRT -L$_NIX_GCC_SLIB -B$_NIX_GLIBC_LIB -B$_NIX_GCC_CRT "\$@"
EOF
            chmod +x "$_ICX_WRAPPER"

            export CC="$_ICX_WRAPPER"
            export CXX=${pkgs.intel-oneapi.hpc}/bin/icpx
            export FC=${pkgs.intel-oneapi.hpc}/bin/ifx
            export PATH=${pkgs.intel-oneapi.hpc}/bin:$PATH
            export LD_LIBRARY_PATH=${pkgs.intel-oneapi.hpc}/lib:$_NIX_GLIBC_LIB:$_NIX_GCC_CRT:$_NIX_GCC_SLIB:$LD_LIBRARY_PATH

            echo "h5fortran: Intel oneAPI (ifx/icx) + HDF5 + CMake + Ninja"
            echo "CC: $CC  FC: $FC"
            echo "HDF5: ${pkgs.hdf5-fortran.dev}"
          '';

          HDF5_DIR = "${pkgs.hdf5-fortran.dev}";
        };

        devShells.coverage = mkGccShell {
          name = "h5fortran-coverage";
          gcc-pkg = pkgs.gcc14;
          gfortran-pkg = pkgs.gfortran14;
          packages = with pkgs; [
            gcovr
            lcov
          ];
          shellHookExtra = ''
            echo "Tools: gcovr, lcov"
            echo "Build: cmake -Bbuild -G Ninja --preset coverage"
          '';
        };

        devShells.shared = mkGccShell {
          name = "h5fortran-shared";
          gcc-pkg = pkgs.gcc14;
          gfortran-pkg = pkgs.gfortran14;
          shellHookExtra = ''
            echo "Build: cmake -Bbuild -G Ninja --workflow --preset shared"
          '';
        };

        packages = { };
      }
    );
}
