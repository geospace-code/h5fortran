#!/usr/bin/env python3
"""
Compile HDF5 library

Be sure environment variables are set for your desired compiler.
Use the full compiler path if it's not getting the right compiler.

* FC: Fortran compiler name or path
* CC: C compiler name or path
"""
import typing as T
import sys
import os
import subprocess
import shutil
import argparse
import tempfile
from pathlib import Path

# ========= user parameters ======================
BUILDDIR = "build"
HDF5_TAG = "hdf5-1_10_6"


# ========= end of user parameters ================

nice = ["nice"] if sys.platform == "linux" else []


def cli():
    p = argparse.ArgumentParser(description="Compile HDF5 library")
    p.add_argument(
        "compiler",
        help="compiler to build libraries for",
        choices=["gcc", "intel", "ibmxl"],
    )
    p.add_argument("-prefix", help="top-level directory to install libraries under")
    p.add_argument(
        "-workdir",
        help="top-level directory to build under (can be deleted when done)",
        default=tempfile.gettempdir(),
    )
    P = p.parse_args()

    compiler = P.compiler

    prefix = P.prefix if P.prefix else f"~/lib_{P.compiler}"

    if compiler == "gcc":
        env = gcc_compilers()
    elif compiler == "intel":
        env = intel_compilers()
    elif compiler == "ibmxl":
        env = ibmxl_compilers()
    else:
        raise ValueError(f"unknown compiler {compiler}")

    hdf5(
        {"prefix": Path(prefix).expanduser(), "workdir": Path(P.workdir).expanduser()},
        env=env,
    )


def hdf5(dirs: T.Dict[str, Path], env: T.Mapping[str, str]):
    """ build and install HDF5
    some systems have broken libz and so have trouble extracting tar.bz2 from Python.
    To avoid this, we git clone the release instead.
    """

    if os.name == "nt":
        if "ifort" in env["FC"]:
            msg = """
For Windows with Intel compiler, use HDF5 binaries from HDF Group.
https://www.hdfgroup.org/downloads/hdf5/
look for filename like hdf5-1.12.0-Std-win10_64-vs14-Intel.zip
            """
        elif "gfortran" in env["FC"]:
            msg = """
For MSYS2 on Windows, just use MSYS2 HDF5.
Install from the MSYS2 terminal like:
pacman -S mingw-w64-x86_64-hdf5
reference: https://packages.msys2.org/package/mingw-w64-x86_64-hdf5
            """
        else:
            msg = """
For Windows, use HDF5 binaries from HDF Group.
https://www.hdfgroup.org/downloads/hdf5/
Instead of this, it is generally best to use MSYS2 or Windows Subsystem for Linux
            """
        raise NotImplementedError(msg)

    hdf5_name = "hdf5"
    install_dir = dirs["prefix"] / hdf5_name
    source_dir = dirs["workdir"] / hdf5_name

    git_url = "https://bitbucket.hdfgroup.org/scm/hdffv/hdf5.git"

    git_download(source_dir, git_url, HDF5_TAG)

    # cmd = [
    #     "./configure",
    #     f"--prefix={install_dir}",
    #     "--enable-fortran",
    #     "--enable-build-mode=production",
    # ]

    cmd = [
        "cmake",
        f"-S{source_dir}",
        f"-B{source_dir/BUILDDIR}",
        f"-DCMAKE_INSTALL_PREFIX={install_dir}",
        "-DBUILD_SHARED_LIBS:BOOL=false",
        "-DCMAKE_BUILD_TYPE=Release",
        "-DHDF5_BUILD_FORTRAN:BOOL=true",
        "-DHDF5_BUILD_CPP_LIB:BOOL=false",
        "-DHDF5_BUILD_TOOLS:BOOL=false",
        "-DBUILD_TESTING:BOOL=false",
        "-DHDF5_BUILD_EXAMPLES:BOOL=false",
    ]
    subprocess.check_call(nice + cmd, env=env)

    cmd = ["cmake", "--build", BUILDDIR, "--parallel"]
    subprocess.check_call(nice + cmd, cwd=source_dir)

    # cmd = ["make", "-j", "install"]
    cmd = ["cmake", "--install", BUILDDIR, "--parallel"]
    subprocess.check_call(nice + cmd, cwd=source_dir)


def git_download(path: Path, repo: str, tag: str):
    """
    Use Git to download code repo.
    """
    GITEXE = shutil.which("git")

    if not GITEXE:
        raise FileNotFoundError("Git not found.")

    git_version = (
        subprocess.check_output([GITEXE, "--version"], universal_newlines=True)
        .strip()
        .split()[-1]
    )
    print("Using Git", git_version)

    if path.is_dir():
        # don't use "git -C" for old HPC
        ret = subprocess.run([GITEXE, "checkout", tag], cwd=str(path))
        if ret.returncode != 0:
            ret = subprocess.run([GITEXE, "fetch"], cwd=str(path))
            if ret.returncode != 0:
                raise RuntimeError(
                    f"could not fetch {path}  Maybe try removing this directory."
                )
            subprocess.check_call([GITEXE, "checkout", tag], cwd=str(path))
    else:
        # shallow clone
        if tag:
            subprocess.check_call(
                [
                    GITEXE,
                    "clone",
                    repo,
                    "--branch",
                    tag,
                    "--single-branch",
                    str(path),
                ]
            )
        else:
            subprocess.check_call([GITEXE, "clone", repo, "--depth", "1", str(path)])


def get_compilers(compiler_name: str, **kwargs) -> T.Mapping[str, str]:
    """ get paths to compilers

    Parameters
    ----------

    compiler_name: str
        arbitrary string naming compiler--to give useful error message when compiler not found.
    """
    env = os.environ

    for k, v in kwargs.items():
        c = env.get(k, "")
        if v not in c:
            c = shutil.which(v)
        if not c:
            raise FileNotFoundError(
                f"Compiler {compiler_name} was not found: {k}."
                " Did you load the compiler shell environment first?"
            )
        env.update({k: c})

    return env


def gcc_compilers() -> T.Mapping[str, str]:
    return get_compilers("GNU", FC="gfortran", CC="gcc", CXX="g++")


def intel_compilers() -> T.Mapping[str, str]:
    return get_compilers(
        "Intel",
        FC="ifort",
        CC="icl" if os.name == "nt" else "icc",
        CXX="icl" if os.name == "nt" else "icpc",
    )


def ibmxl_compilers() -> T.Mapping[str, str]:
    return get_compilers("IBM XL", FC="xlf", CC="xlc", CXX="xlc++")


if __name__ == "__main__":
    cli()
