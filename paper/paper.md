---
title: 'h5fortran: object-oriented polymorphic Fortran interface for HDF5 file IO'
tags:
authors:
  - name: Michael Hirsch
    orcid: 0000-0002-1637-6526
    affiliation: "1"
affiliations:
 - name: Boston University
   index: 1
date: 30 March 2020
bibliography: paper.bib
---

# Summary

Fortran has only raw file input-output (IO) built in to the language.
To support reproducibility of work done in any programming language and long-term usefulness of the data generated or processed, it is beneficial to use self-describing data file formats like HDF5 [@2011hdf5].
Many popular languages and libraries used for simulation and data science use the HDF5 [@hdf5] file IO library.
Most programs and libraries intended for use by practitioners such as modelers and data scientists themselves use an object-oriented HDF5 interface like h5py [@h5py] [@h5pybook].

h5fortran [@h5fortran] is a Fortran 2008 interface to HDF5 that abstracts away most details of a frequently-used subset of HDF5 operations.
h5fortran makes HDF5 use from Fortran as easy as in high-level scripting languages.
h5fortran is known to work for Gfortran 6 or newer and Intel Fortran 19.0 or newer for Linux, MacOS, Windows on Intel / AMD, ARM and IBM POWER systems.
The Fortran 2008 standard is adhered to by h5fortran, so the main limit to Fortran compiler support is the compiler supporting Fortran 2008 standard.

h5fortran has general applicability to projects needing to do any of:

* writing variables to HDF5: scalar to 7-D, of type real32, real64 or integer
* reading variables from HDF5: scalar to 7-D, of type real32, real64 or integer
* reading or writing character variables to / from HDF5 file
* writing variable attributes to disk
* getting the shape of a disk variable, for example to allocate a memory variable to read that variable

In addition to the object-oriented interface, h5fortran provides single-command read / write procedures.
Array slicing on read allows reading a portion of a large disk variable into memory.
If the user has HDF5 with SZIP or ZLIB compression enabled, h5fortran is capable of reading and writing compressed variables, which can save over 50% disk space depending on the data lossless compressibility.
Data shuffling and Fletcher32 checksums provide better compression and a check of file integrity respectively.
h5fortran was designed for use by individual users on their laptops or embedded devices, as well as for use in HPC applications where parallel tasks need read only part of a milestone or shared HDF5 variable.

h5fortran was originally developed for the GEMINI [@gemini3d] [@zettergren] ionospheric model, funded in part by NASA ROSES \#19-HDEE19_2-0007.

## Other programs

While other HDF5 interfaces exist, h5fortran presents a broad set of commonly used features, comprehensive test coverage and robustness across compilers and computing systems.
We have written a companion library for NetCDF4 called nc4fortran [@nc4fortran], which by design has a nearly identical user-facing API.
Other Fortran HDF5 interfaces such as HDF5_Utils [@hdf5_utils] use a functional interface mimicking the HDF5 LT functions, which require the end user to keep track of extra variables versus the single object used by h5fortran.
A package for C++ with similar priorities of using modern language features and simple commands is h5pp [@h5pp].
