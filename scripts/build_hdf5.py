#!/usr/bin/env python3

"""
Compile HDF5 library

Be sure environment variables are set for your desired compiler.
Use the full compiler path if it's not getting the right compiler.

* FC: Fortran compiler name or path
* CC: C compiler name or path
"""

from __future__ import annotations
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
import tarfile
import json

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
    p.add_argument(
        "-parallel", help="build HDF5 with MPI parallel support", action="store_true"
    )
    p.add_argument("-debug", help="debug configure CMake", action="store_true")
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

    # HDF5 install fails to link if prior HDF5 library version mixed in
    if (dirs["prefix"] / "include/hdf5.h").is_file():
        raise FileExistsError(
            f"""
HDF5 library already installed under:
{dirs['prefix']}
Please pick a new install location or completely remove the old HDF5 install directory.
Otherwise, HDF5 will fail to link correctly with prior version and this version mixed."""
        )

    urls = json.loads(JSON_FILE.read_text())

    dirs["zlib"] = zlib(dirs, urls["zlib2"], env=env, debug=P.debug)

    hdf5(
        dirs,
        urls["hdf5"],
        env=env,
        download_git=P.git,
        parallel=P.parallel,
        debug=P.debug,
    )


def zlib(
    dirs: dict[str, Path],
    urls: dict[str, str],
    *,
    env: T.Mapping[str, str],
    debug: bool = False,
) -> Path:
    zlib_filename = urls["url"].split("/")[-1]
    install_dir = dirs["prefix"]

    source_dir = dirs["workdir"] / f"zlib-ng-{zlib_filename[:5]}"
    # FIXME: make source_dir more programmatic by introspecting tar file and
    # looking for CMakeLists.txt. I think this is what CMake itself does via ExternalProject.
    build_dir = source_dir / BUILDDIR

    zlib_archive = dirs["workdir"] / zlib_filename
    url_retrieve(urls["url"], zlib_archive, filehash=("sha256", urls["sha256"]))

    if not (source_dir / "CMakeLists.txt").is_file():
        with tarfile.open(zlib_archive) as z:
            z.extractall(dirs["workdir"])

    cmd0 = [
        "cmake",
        f"-S{source_dir}",
        f"-B{build_dir}",
        f"--install-prefix={install_dir}",
        "-DCMAKE_BUILD_TYPE=Release",
        "-DZLIB_COMPAT:BOOL=true",
        "-DBUILD_SHARED_LIBS:BOOL=off",
        "-DZLIB_ENABLE_TESTS:BOOL=off",
    ]

    if debug:
        cmd0.append("--debug-find")

    cmd1 = ["cmake", "--build", str(build_dir), "--parallel"]

    cmd2 = ["cmake", "--install", str(build_dir)]

    subprocess.check_call(cmd0, env=env)
    subprocess.check_call(cmd1)
    subprocess.check_call(cmd2)

    return install_dir


def hdf5(
    dirs: dict[str, Path],
    urls: dict[str, str],
    env: dict[str, str],
    *,
    download_git: bool = False,
    parallel: bool = False,
    debug: bool = False,
):
    """build and install HDF5

    Git works, but we use release for stability/download speed
    """

    name = "hdf5"
    use_cmake = True
    source_dir = dirs["workdir"] / name

    if os.name == "nt" and Path(env["CC"]).stem == "icl":
        zlib_filename = "zlibstatic.lib"
    else:
        zlib_filename = "libz.a"

    if download_git:
        git_download(source_dir, urls["git"], urls["tag"])
    else:
        url = urls["url"]
        name += "-" + url.split("/")[-1].split(".")[0]

        source_dir = source_dir.with_name(name)

        archive = dirs["workdir"] / url.split("/")[-1]
        url_retrieve(url, archive, filehash=("sha256", urls["sha256"]))

        if not (source_dir / "CMakeLists.txt").is_file():
            with tarfile.open(archive) as z:
                z.extractall(dirs["workdir"])

    install_dir = dirs["prefix"]
    build_dir = source_dir / BUILDDIR
    env["ZLIB_ROOT"] = str(dirs["zlib"])

    if use_cmake or os.name == "nt":
        # works for Intel oneAPI on Windows and many other systems/compilers.
        # works for Make or Ninja in general.
        cmd0 = [
            "cmake",
            f"-S{source_dir}",
            f"--install-prefix={install_dir}",
            "-DHDF5_GENERATE_HEADERS:BOOL=false",
            "-DHDF5_DISABLE_COMPILER_WARNINGS:BOOL=true",
            "-DBUILD_STATIC_LIBS:BOOL=true",
            "-DBUILD_SHARED_LIBS:BOOL=false",
            "-DCMAKE_BUILD_TYPE=Release",
            "-DHDF5_BUILD_FORTRAN:BOOL=true",
            "-DHDF5_BUILD_CPP_LIB:BOOL=false",
            "-DBUILD_TESTING:BOOL=false",
            "-DHDF5_BUILD_EXAMPLES:BOOL=false",
            f"-DZLIB_LIBRARY:FILEPATH={dirs['zlib']}/lib/{zlib_filename}",
            f"-DZLIB_INCLUDE_DIR:PATH={dirs['zlib']}/include",
            "-DHDF5_ENABLE_Z_LIB_SUPPORT:BOOL=true",
            "-DZLIB_USE_EXTERNAL:BOOL=false",
            "-DUSE_LIBAEC:bool=true"
            # the options below didn't work for building with HDF5 1.10.7
            # "-DZLIB_USE_EXTERNAL:BOOL=true"
            # "-DHDF5_ENABLE_Z_LIB_SUPPORT:BOOL=true",
            # "-DHDF5_ALLOW_EXTERNAL_SUPPORT=GIT",
            # "-DZLIB_GIT_URL=https://github.com/madler/zlib.git",
        ]

        if parallel:
            cmd0 += [
                "-DHDF5_ENABLE_PARALLEL:BOOL=true",
                "-DHDF5_BUILD_TOOLS:BOOL=false",
            ]
            # https://github.com/HDFGroup/hdf5/issues/818  for broken ph5diff
        else:
            cmd0 += [
                "-DHDF5_ENABLE_PARALLEL:BOOL=false",
                "-DHDF5_BUILD_TOOLS:BOOL=true",
            ]

        if debug:
            cmd0.append("--debug-find")

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
    url: str, outfile: Path, filehash: tuple[str, str] = None, overwrite: bool = False
):
    """
    Parameters
    ----------
    url: str
        URL to download from
    outfile: pathlib.Path
        output filepath (including name)
    filehash: tuple of str, str
        hash type (md5, sha256, etc.) and hash
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
