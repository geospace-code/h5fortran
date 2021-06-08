#!/usr/bin/env python3

"""
Compile HDF5 library

Be sure environment variables are set for your desired compiler.
Use the full compiler path if it's not getting the right compiler.

* FC: Fortran compiler name or path
* CC: C compiler name or path
"""

import typing as T
import os
import subprocess
import shutil
import argparse
import tempfile
from pathlib import Path
import urllib.request
import hashlib
import urllib.error
import socket
import zipfile
import json
import sys

if sys.version_info < (3, 6, 2):
    raise RuntimeError("Python >= 3.6.2 required")

# ========= user parameters ======================
BUILDDIR = "build"
JSON_FILE = Path(__file__).resolve().parents[1] / "cmake/libraries.json"
# ========= end of user parameters ================


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
    p.add_argument(
        "-git",
        help="use current HDF5 git revision instead of download",
        action="store_true",
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

    dirs = {
        "prefix": Path(prefix).expanduser(),
        "workdir": Path(P.workdir).expanduser(),
    }

    urls = json.loads(JSON_FILE.read_text())

    dirs["zlib"] = zlib(dirs, urls["zlib2"], env=env)

    hdf5(dirs, urls["hdf5"], env=env, download_git=P.git)


def zlib(
    dirs: T.Dict[str, Path], urls: T.Dict[str, str], env: T.Mapping[str, str]
) -> Path:
    name = "zlib-1.2.11"
    zlib_filename = urls["url"].split("/")[-1]

    install_dir = dirs["prefix"] / name
    source_dir = dirs["workdir"] / name
    build_dir = source_dir / BUILDDIR

    zlib_archive = dirs["workdir"] / zlib_filename

    url_retrieve(urls["url"], zlib_archive, filehash=["sha1", urls["sha1"]])

    if not (source_dir / "CMakeLists.txt").is_file():
        with zipfile.ZipFile(zlib_archive) as z:
            z.extractall(dirs["workdir"])

    cmd0 = [
        "cmake",
        f"-S{source_dir}",
        f"-B{build_dir}",
        f"-DCMAKE_INSTALL_PREFIX={install_dir}",
        "-DCMAKE_BUILD_TYPE=Release",
    ]

    cmd1 = ["cmake", "--build", str(build_dir), "--parallel"]

    cmd2 = ["cmake", "--install", str(build_dir)]

    subprocess.check_call(cmd0, env=env)
    subprocess.check_call(cmd1)
    subprocess.check_call(cmd2)

    return install_dir


def hdf5(
    dirs: T.Dict[str, Path],
    urls: T.Dict[str, str],
    env: T.Dict[str, str],
    download_git: bool = False,
):
    """build and install HDF5

    Git works, but we use release for stability/download speed
    """

    use_cmake = True

    name = "hdf5"
    source_dir = dirs["workdir"] / name

    if os.name == "nt":
        if Path(env["CC"]).stem == "icl":
            zlib_filename = "zlibstatic.lib"
        else:
            zlib_filename = "libzlibstatic.a"
    else:
        zlib_filename = "libz.a"

    if download_git:
        git_download(source_dir, urls["git"], urls["tag"])
    else:
        url = urls["url"]
        name += "-" + url.split("/")[-1].split(".")[0]

        source_dir = source_dir.with_name(name)

        archive = dirs["workdir"] / url.split("/")[-1]
        url_retrieve(url, archive, filehash=["sha1", urls["sha1"]])

        if not (source_dir / "CMakeLists.txt").is_file():
            with zipfile.ZipFile(archive) as z:
                z.extractall(dirs["workdir"])

    install_dir = dirs["prefix"] / name
    build_dir = source_dir / BUILDDIR
    env["ZLIB_ROOT"] = str(dirs["zlib"])

    if use_cmake or os.name == "nt":
        # works for Intel oneAPI on Windows and many other systems/compilers.
        # works for Make or Ninja in general.
        cmd0 = [
            "cmake",
            f"-S{source_dir}",
            f"-DCMAKE_INSTALL_PREFIX={install_dir}",
            "-DHDF5_GENERATE_HEADERS:BOOL=false",
            "-DHDF5_DISABLE_COMPILER_WARNINGS:BOOL=true",
            "-DBUILD_SHARED_LIBS:BOOL=false",
            "-DCMAKE_BUILD_TYPE=Release",
            "-DHDF5_BUILD_FORTRAN:BOOL=true",
            "-DHDF5_BUILD_CPP_LIB:BOOL=false",
            "-DHDF5_BUILD_TOOLS:BOOL=false",
            "-DBUILD_TESTING:BOOL=false",
            "-DHDF5_BUILD_EXAMPLES:BOOL=false",
            f"-DZLIB_LIBRARY:FILEPATH={dirs['zlib']}/lib/{zlib_filename}",
            f"-DZLIB_INCLUDE_DIR:PATH={dirs['zlib']}/include",
            "-DHDF5_ENABLE_Z_LIB_SUPPORT:BOOL=true",
            "-DZLIB_USE_EXTERNAL:BOOL=false",
            # these options below didn't work for building with HDF5 1.10.7
            # "-DZLIB_USE_EXTERNAL:BOOL=true"
            # "-DHDF5_ENABLE_Z_LIB_SUPPORT:BOOL=true",
            # "-DHDF5_ALLOW_EXTERNAL_SUPPORT=GIT",
            # "-DZLIB_GIT_URL=https://github.com/madler/zlib.git",
        ]

        cmd1 = ["cmake", "--build", str(build_dir), "--parallel"]

        cmd2 = ["cmake", "--install", str(build_dir)]

        # this old "cmake .." style command is necessary due to bugs with
        # HDF5 (including 1.10.7) CMakeLists:
        #   CMake Error at config/cmake/HDF5UseFortran.cmake:205 (file):
        #   file failed to open for reading (No such file or directory):
        #   C:/Users/micha/AppData/Local/Temp/hdf5/build/pac_fconftest.out.
        build_dir.mkdir(exist_ok=True)
        subprocess.check_call(cmd0, cwd=build_dir, env=env)
    else:
        cmd0 = [
            "./configure",
            f"--prefix={install_dir}",
            "--enable-fortran",
            "--enable-build-mode=production",
        ]
        cmd1 = ["make", "-j"]
        cmd2 = ["make", "-j", "install"]
        subprocess.check_call(cmd0, cwd=source_dir, env=env)

    subprocess.check_call(cmd1, cwd=source_dir)
    subprocess.check_call(cmd2, cwd=source_dir)


def git_download(path: Path, repo: str, tag: str):
    """
    Use Git to download code repo.
    """
    GITEXE = shutil.which("git")

    if not GITEXE:
        raise FileNotFoundError("Git not found.")

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
                [GITEXE, "clone", repo, "--branch", tag, "--single-branch", str(path)]
            )
        else:
            subprocess.check_call([GITEXE, "clone", repo, "--depth", "1", str(path)])


def url_retrieve(
    url: str, outfile: Path, filehash: T.Sequence[str] = None, overwrite: bool = False
):
    """
    Parameters
    ----------
    url: str
        URL to download from
    outfile: pathlib.Path
        output filepath (including name)
    filehash: tuple of str, str
        hash type (md5, sha1, etc.) and hash
    overwrite: bool
        overwrite if file exists
    """

    outfile = Path(outfile).expanduser().resolve()
    if outfile.is_dir():
        raise ValueError("Please specify full filepath, including filename")
    # need .resolve() in case intermediate relative dir doesn't exist
    if overwrite or not outfile.is_file():
        outfile.parent.mkdir(parents=True, exist_ok=True)
        print(f"{url} => {outfile}")
        try:
            urllib.request.urlretrieve(url, str(outfile))
        except (socket.gaierror, urllib.error.URLError) as err:
            raise ConnectionError(f"could not download {url} due to {err}")

    if filehash and filehash[1]:
        if not file_checksum(outfile, filehash[0], filehash[1]):
            raise ValueError(f"Hash mismatch: {outfile}")


def file_checksum(fn: Path, mode: str, filehash: str) -> bool:
    h = hashlib.new(mode)
    h.update(fn.read_bytes())
    return h.hexdigest() == filehash


def get_compilers(compiler_name: str, **kwargs) -> T.Mapping[str, str]:
    """get paths to compilers

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
