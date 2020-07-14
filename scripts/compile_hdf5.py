#!/usr/bin/env python3
"""
Compile HDF5 library

Be sure environment variables are set for your desired compiler.
Use the full compiler path if it's not getting the right compiler.

* FC: Fortran compiler name or path
* CC: C compiler name or path
"""
import subprocess
import shutil
import tempfile
import logging
from pathlib import Path
from argparse import ArgumentParser
import typing as T
import sys
import os

# ========= user parameters ======================
BUILDDIR = "build"
HDF5_TAG = "1.12/master"
HDF5_GIT = "https://bitbucket.hdfgroup.org/scm/hdffv/hdf5.git"


# ========= end of user parameters ================

nice = ["nice"] if sys.platform == "linux" else []


def cli():
    p = ArgumentParser(description="Compile HDF library")
    p.add_argument(
        "compiler",
        help="compiler to build libraries for",
        choices=["gcc", "intel", "ibmxl"],
    )
    p.add_argument("-prefix", help="toplevel path to install libraries under")
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

    hdf5({"prefix": prefix, "workdir": P.workdir}, env)


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
        raise SystemExit(msg)

    hdf5_name = "hdf5"
    install_dir = dirs["prefix"] / hdf5_name
    source_dir = dirs["workdir"] / hdf5_name

    git_update(source_dir, HDF5_GIT, tag=HDF5_TAG)

    cmd = [
        "./configure",
        f"--prefix={install_dir}",
        "--enable-fortran",
        "--enable-build-mode=production",
    ]

    subprocess.check_call(nice + cmd, cwd=source_dir, env=env)

    cmd = ["make", "-C", str(source_dir), "-j", "install"]
    subprocess.check_call(nice + cmd)


def git_update(path: Path, repo: str, tag: str = None):
    """
    Use Git to update a local repo, or clone it if not already existing.

    we use cwd= instead of "git -C" for very old Git versions that might be on your HPC.
    """
    GITEXE = shutil.which("git")

    if not GITEXE:
        logging.error("Git not available.")
        return

    if path.is_dir():
        subprocess.check_call([GITEXE, "-C", str(path), "pull"])
    else:
        # shallow clone
        if tag:
            subprocess.check_call(
                [
                    GITEXE,
                    "clone",
                    repo,
                    "--depth",
                    "1",
                    "--branch",
                    tag,
                    "--single-branch",
                    str(path),
                ]
            )
        else:
            subprocess.check_call([GITEXE, "clone", repo, "--depth", "1", str(path)])


def get_compilers(**kwargs) -> T.Mapping[str, str]:
    """ get paths to compilers """
    env = os.environ

    for k, v in kwargs.items():
        c = env.get(k, "")
        if v not in c:
            c = shutil.which(v)
        if not c:
            raise FileNotFoundError(v)
        env.update({k: c})

    return env


def gcc_compilers() -> T.Mapping[str, str]:
    return get_compilers(FC="gfortran", CC="gcc", CXX="g++")


def intel_compilers() -> T.Mapping[str, str]:
    return get_compilers(
        FC="ifort",
        CC="icl" if os.name == "nt" else "icc",
        CXX="icl" if os.name == "nt" else "icpc",
    )


def ibmxl_compilers() -> T.Mapping[str, str]:
    return get_compilers(FC="xlf", CC="xlc", CXX="xlc++")


if __name__ == "__main__":
    cli()
