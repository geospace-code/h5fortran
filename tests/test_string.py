#!/usr/bin/env python3
"""
write strings to ensure h5fortran can read
"""

import argparse
from pathlib import Path

import h5py

p = argparse.ArgumentParser(description="write test strings in HDF5")
p.add_argument("file", help="hDF5 file to write")
P = p.parse_args()

fn = Path(P.file).expanduser()

with h5py.File(fn, "w") as f:
    f["/variable"] = "Hello World!"
    f.create_dataset("/nullpad", dtype="S40", data="Hello World!")
