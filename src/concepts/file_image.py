#!/usr/bin/env python3
"""
Demonstrates low level h5py HDF5 file_image API

from h5py source code.
"""

from binascii import a2b_base64
from zlib import decompress

import h5py
from h5py import h5f, h5p

compressed_image = "eJzr9HBx4+WS4mIAAQ4OBhYGAQZk8B8KKjhQ+TD5BCjNCKU7oPQKJpg4I1hOAiouCDUfXV1IkKsrSPV/NACzx4AFQnMwjIKRCDxcHQNAdASUD0ulJ5hQ1ZWkFpeAaFh69KDQXkYGNohZjDA+JCUzMkIEmKHqELQAWKkAByytOoBJViAPJM7ExATWyAE0B8RgZkyAJmlYDoEAIahukJoNU6+HMTA0UOgT6oBgP38XUI6G5UMFZrzKR8EoGAUjGMDKYVgxDSsuAHcfMK8="


def test_file(tmp_path):
    image = decompress(a2b_base64(compressed_image))

    # FAPL: File access property list
    fapl = h5p.create(h5py.h5p.FILE_ACCESS)
    fapl.set_fapl_core()
    fapl.set_file_image(image)

    file = tmp_path / "disk.h5"
    fid = h5f.open(bytes(file), h5py.h5f.ACC_RDONLY, fapl=fapl)
    with h5py.File(fid) as f:
        assert (f["test"][:] == [1, 2, 3]).all()
        assert f["test"].dtype == "int64"


def test_image():
    image = decompress(a2b_base64(compressed_image))

    fid = h5f.open_file_image(image)

    with h5py.File(fid) as f:
        assert (f["test"][:] == [1, 2, 3]).all()
        assert f["test"].dtype == "int64"
