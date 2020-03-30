"""
Utilities to download and extract zip and tar files
"""
import hashlib
import urllib.request
import urllib.error
import socket
import zipfile
import tarfile
import typing as T
from pathlib import Path

Pathlike = T.Union[str, Path]


def url_retrieve(url: str, outfile: Pathlike, filehash: T.Sequence[str] = None, overwrite: bool = False):
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

    if filehash:
        if not file_checksum(outfile, filehash[0], filehash[1]):
            raise ValueError(f"Hash mismatch: {outfile}")


def file_checksum(fn: Path, mode: str, filehash: str) -> bool:
    """
    check has of a file

    Parameters
    ----------
    fn: pathlib.Path
        filename to check
    mode: str
        hash name e.g. md5
    filehash: str
        expected hash of file

    Returns
    -------

    hash_ok: bool
        true if file hash matches
    """
    h = hashlib.new(mode)
    h.update(fn.read_bytes())
    return h.hexdigest() == filehash


def extract_zip(fn: Pathlike, outpath: Pathlike, overwrite: bool = False):
    """
    extracts a zip file archive

    Parameters
    ----------

    fn: pathlib.Path
        archive filepath
    outpath: pathlib.Path
        directory to extract files into
    overwrite: bool, optional
        if true overwrite output
    """
    outpath = Path(outpath).expanduser().resolve()
    # need .resolve() in case intermediate relative dir doesn't exist
    if outpath.is_dir() and not overwrite:
        return

    fn = Path(fn).expanduser().resolve()
    with zipfile.ZipFile(fn) as z:
        z.extractall(str(outpath.parent))


def extract_tar(fn: Pathlike, outpath: Pathlike, overwrite: bool = False):
    """
    extracts a tar file archive

    Parameters
    ----------

    fn: pathlib.Path
        archive filepath
    outpath: pathlib.Path
        directory to extract files into
    overwrite: bool, optional
        if true overwrite output
    """
    outpath = Path(outpath).expanduser().resolve()
    # need .resolve() in case intermediate relative dir doesn't exist
    if outpath.is_dir() and not overwrite:
        return

    fn = Path(fn).expanduser().resolve()
    if not fn.is_file():
        # tarfile gives confusing error on missing file
        raise FileNotFoundError(fn)
    with tarfile.open(fn) as z:
        z.extractall(str(outpath.parent))
