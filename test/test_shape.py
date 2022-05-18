#!/usr/bin/env python3
"""
read output of test_shape.f90 and see if it's the right shape
in a few coding languages
"""

import argparse
from pathlib import Path

import h5py

p = argparse.ArgumentParser(description="analyze output of test_shape.f90")
p.add_argument("file", help="hDF5 file to analyze")
P = p.parse_args()

fn = Path(P.file).expanduser()

var = "/d7"
f_order = (2, 1, 3, 4, 7, 6, 5)
c_order = f_order[::-1]


if not fn.is_file():
    raise FileNotFoundError(fn)

with h5py.File(fn, "r") as f:
    if f[var].shape != c_order:
        raise ValueError(f"h5py: expected {c_order} but got {f[var].shape}")

print("OK: Python h5py")
