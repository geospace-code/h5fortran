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
from pathlib import Path
from argparse import ArgumentParser
import typing as T
import sys
import os

from web import url_retrieve, extract_tar

# ========= user parameters ======================
BUILDDIR = "build"
HDF5VERSION = "1.12.0"
HDF5URL = f"https://zenodo.org/record/3700903/files/hdf5-{HDF5VERSION}.tar.bz2?download=1"
HDF5MD5 = "1fa68c4b11b6ef7a9d72ffa55995f898"

# ========= end of user parameters ================

nice = ["nice"] if sys.platform == "linux" else []


def hdf5(prefix: Path, workdir: Path):
    """
    build and install HDF5

    Parameters
    ----------

    prefix: pathlib.Path
        where to install the library
    workdir: pathlib.Path
        where to keep the source files when building
    """
    if os.name == "nt":
        raise NotImplementedError("Please use binaries from HDF Group for Windows appropriate for your compiler.")

    prefix = Path(prefix).expanduser().resolve()
    workdir = Path(workdir).expanduser().resolve()

    hdf5_dir = f"hdf5-{HDF5VERSION}"
    install_dir = prefix / hdf5_dir
    source_dir = workdir / hdf5_dir

    tarfn = workdir / f"hdf5-{HDF5VERSION}.tar.bz2"
    url_retrieve(HDF5URL, tarfn, ("md5", HDF5MD5))
    extract_tar(tarfn, source_dir)

    cmd = nice + ["./configure", f"--prefix={install_dir}", "--enable-fortran", "--enable-build-mode=production"]

    subprocess.check_call(cmd, cwd=source_dir)

    cmd = nice + ["make", "-C", str(source_dir), "-j", "install"]
    subprocess.check_call(cmd)


def cmake_build(args: T.List[str], source_dir: Path, build_dir: Path):
    """ build and install with CMake """
    cmake = shutil.which("cmake")
    if not cmake:
        raise EnvironmentError("CMake not found.")

    subprocess.check_call(nice + [cmake] + args + ["-B", str(build_dir), "-S", str(source_dir)])

    subprocess.check_call(nice + [cmake, "--build", str(build_dir), "--parallel", "--target", "install"])


if __name__ == "__main__":
    p = ArgumentParser()
    p.add_argument("-prefix", help="toplevel path to install libraries under", default="~/lib")
    p.add_argument("-workdir", help="toplevel path to where you keep code repos", default="~/code")
    P = p.parse_args()

    hdf5(P.prefix, P.workdir)
