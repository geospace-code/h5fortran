#!/usr/bin/env python3
"""
write attributes to ensure h5fortran can read
"""

import argparse
from pathlib import Path

import numpy as np
import h5py

p = argparse.ArgumentParser(description="write test attributes in HDF5")
p.add_argument("file", help="HDF5 file to write")
P = p.parse_args()

fn = Path(P.file).expanduser()

with h5py.File(fn, "w") as f:
    v = f.create_dataset("/empty", dtype=float)

    v.attrs["real32-scalar"] = np.float32(3.0)
    v.attrs["real64-scalar"] = np.float64(3.0)
    v.attrs["real16-scalar"] = np.float16(3.0)

    v.attrs["variable_str"] = "Hi there"
    v.attrs["variable_str1"] = ["eight", "nine"]
    v.attrs["variable_str2"] = [
        ["eight", "nine", "ten"],
        ["eleven", "twelve", "thirteen"],
    ]

    v.attrs.create("nullpad", dtype=h5py.string_dtype("utf-8", 40), data="Hello World!")
    v.attrs.create(
        "nullpad1", dtype=h5py.string_dtype("utf-8", 20), data=["two", "three", "four"]
    )
    v.attrs.create(
        "nullpad2",
        dtype=h5py.string_dtype("utf-8", 20),
        data=[["eight", "nine", "ten"], ["eleven", "twelve", "thirteen"]],
    )

    v.attrs["smiley"] = "😀"
    v.attrs["wink"] = "😉"
