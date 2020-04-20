# Spack Configuration

This file contains the instruction on how to build mini Spack version for PACE-ProvBench testsuite. 

## Steps to build software 

**step 1 is needed to run before other steps**

1. run `./setup-spack.sh <user defined path>`. 
Contributor can specify the path where test suite will be build under `<user defined path>`. 
if nothing is specified, the default path is `<repo root>/Application/Spack`. 
`$BENCH_SPACK_ROOT` is set to `<user defined path>` or default path.
 the layout of the structure of `$BENCH_SPACK_ROOT` is `$BENCH_SPACK_ROOT/(modules,packages,source)`.
  * modules subdirectory stores all module files generated from spack
  * packages subdirectory stores all binaries generated from spack 
  * source  subdirectory stores spack source code from github repo. 
  
This step checks out spack source code, creates the *modules* and *packages* subdirectories; 
copies the configuration files from *etc* and *var* (in repo) to spack source directory;
and generates an output file called `bench.spack.root` and `spack.module.root` to store the root
location of spack and spack module files for subsequent steps. 

Please note, if newly checked out SPACK source code changed the directory layout, one need to modify
`setup-spack.sh` to make corresponding change on the following lines:
```
cp -r etc/spack/* $SPACK_ROOT/etc/spack/
cp -r  var/spack/environments $SPACK_ROOT/var/spack/
```
  
2. run `./build-gcc.sh` to build `gcc/9.2.0` and `cmake/3.15.1` using RHEL7.6 default `gcc/4.8.5`, after installation, 
one needs to update `etc/spack/compiler.yaml`, and run step 1 to proposage the changes to the source directory. 
If Contributor wants to change the compiler version, one can edit the SPACK env file under
`var/spack/environments/base_gcc/spack.yaml`

3. run `./build-intel.sh` to build `intel/19.0.3` using `gcc/4.8.5` , after installation, one needs to update 
`etc/spack/compiler.yaml`, one can compare the `compiler.yaml.full` and `compiler.yaml.orig` to see what changes 
need to be done. 
Adding compiler definition follows the example below:
```
- compiler:
    extra_rpaths: 
    flags: 
      cflags: -O2
      cxxflags: -O2
      fflags: -O2
      fcflags: -O2
    modules: []
    operating_system: rhel7
    paths:
      cc: /usr/local/pace-apps/spack/packages/0.12/linux-rhel7-x86_64/gcc-4.8.5/intel-parallel-studio-cluster.2019.3-3npq5qw6ri66bcpqgwzpeqjiqaddmlkl/compilers_and_libraries_2019.3.199/linux/bin/intel64/icc
      cxx: /usr/local/pace-apps/spack/packages/0.12/linux-rhel7-x86_64/gcc-4.8.5/intel-parallel-studio-cluster.2019.3-3npq5qw6ri66bcpqgwzpeqjiqaddmlkl/compilers_and_libraries_2019.3.199/linux/bin/intel64/icpc
      f77: /usr/local/pace-apps/spack/packages/0.12/linux-rhel7-x86_64/gcc-4.8.5/intel-parallel-studio-cluster.2019.3-3npq5qw6ri66bcpqgwzpeqjiqaddmlkl/compilers_and_libraries_2019.3.199/linux/bin/intel64/ifort
      fc:  /usr/local/pace-apps/spack/packages/0.12/linux-rhel7-x86_64/gcc-4.8.5/intel-parallel-studio-cluster.2019.3-3npq5qw6ri66bcpqgwzpeqjiqaddmlkl/compilers_and_libraries_2019.3.199/linux/bin/intel64/ifort
    spec: intel@19.0.3
    target: x86_64
```
The paths to `cc`, `cxx`, `f77`, `fc` need to be updated with newly built intel path.  
Then running step 1 to propogate the changes to the source directory. 
If Contributor wants to change the compiler version, one can edit the SPACK env file under
`var/spack/environments/base_intel/spack.yaml`
 
4. run `./build-mvapich.sh` to build `mvapich2/2.3.1` with `intel/19.0.3`. If Contributor wants to change mpi version, 
one can edit the SPACK env file under `var/spack/environments/bench_intel_mvapich/spack.yaml`

5. run `./build-mv2-apps.sh` to build `hdf5, fftw` with `mvapich2`. If Contributor wants to change libraries' version, 
one can edit the SPACK env file under `var/spack/environments/bench_intel19_mv2_external_apps/spack.yaml`



