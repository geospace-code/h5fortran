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


def get_compilers(fc_name: str, cc_name: str, cxx_name: str) -> T.Mapping[str, str]:
    """ get paths to compilers """
    env = os.environ

    fc = env.get("FC", "")
    if fc_name not in fc:
        fc = shutil.which(fc_name)
    if not fc:
        raise FileNotFoundError(fc_name)

    cc = env.get("CC", "")
    if cc_name not in cc:
        cc = shutil.which(cc_name)
    if not cc:
        raise FileNotFoundError(cc_name)

    cxx = env.get("CXX", "")
    if cxx_name not in cxx:
        cxx = shutil.which(cxx_name)
    if not cxx:
        raise FileNotFoundError(cxx_name)

    env.update({"FC": fc, "CC": cc, "CXX": cxx})

    return env


if __name__ == "__main__":
    p = ArgumentParser()
    p.add_argument(
        "-prefix", help="toplevel path to install libraries under", default="~/lib"
    )
    p.add_argument(
        "-workdir", help="toplevel path to where you keep code repos", default="~/code"
    )
    P = p.parse_args()

    env = get_compilers("gfortran", "gcc", "g++")

    hdf5({"prefix": P.prefix, "workdir": P.workdir}, env)
