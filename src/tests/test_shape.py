#!/usr/bin/env python3
import shutil
import subprocess
from pathlib import Path

try:
    import h5py
except ImportError:
    h5py = None

fn = Path("test_shape.h5")
var = "/d7"
f_order = (2, 1, 3, 4, 7, 6, 5)
c_order = f_order[::-1]


if not fn.is_file():
    raise FileNotFoundError(fn)

if shutil.which("h5ls"):
    h5ls = subprocess.check_output(["h5ls", f"{fn}/{var[1:]}"], universal_newlines=True)
    mat = tuple(map(int, h5ls.split("{", 1)[1].split("}", 1)[0].split(",")))
    if mat != c_order:
        raise ValueError(f"h5ls: expected {c_order} but got {mat}")
    print("OK: h5ls")

if h5py is not None:
    with h5py.File(fn, "r") as f:
        if f[var].shape != c_order:
            raise ValueError(f"h5py: expected {c_order} but got {f[var].shape}")
    print("OK: Python h5py")

if shutil.which("octave-cli"):
    cmd = f"dat=load('{fn}'); soct = size(dat.{var}); assert(all(soct == {list(f_order)}), 'expected {f_order}')"
    subprocess.check_call(["octave-cli", cmd])
    print("OK: GNU Octave")

if shutil.which("matlab"):
    cmd = f"i=h5info('{fn}', '{var}'); smat = i.Dataspace.Size; assert(all(smat == {list(f_order)}), 'expected {f_order}')"
    subprocess.check_call(["matlab", "-batch", cmd])
    print("OK: Matlab")
