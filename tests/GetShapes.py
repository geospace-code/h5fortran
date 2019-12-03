#!/usr/bin/env python3
import shutil
import subprocess
import argparse
import sys
import h5py
from pathlib import Path


p = argparse.ArgumentParser()
p.add_argument("fn", help="hdf5 filename")
p.add_argument("var", help="variable name")
P = p.parse_args()

fn = Path(P.fn).expanduser()
if not fn.is_file():
    print(f"{fn} not found", file=sys.stderr)
    raise SystemExit(77)

if shutil.which("h5ls"):
    print("h5ls tool:")
    subprocess.run(["h5ls", f"{fn}/{P.var}"])

with h5py.File(fn, "r") as f:
    print("h5py")
    print(f[P.var].shape)


if shutil.which("octave-cli"):
    print("GNU Octave dims:")
    cmd = f"dat=load('{fn}'); disp(size(dat.{P.var}))"
    print(cmd)
    subprocess.run(["octave-cli", cmd])
    raise SystemExit

if shutil.which("matlab"):
    print("Matlab dims:")
    cmd = f"i=h5info('{fn}', '{P.var}'); disp(i.Dataspace.Size)"
    subprocess.run(["matlab", "-batch", cmd])
