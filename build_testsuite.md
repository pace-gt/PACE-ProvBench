## Build the initial test suite

This instruction is for contributor who sets up the test suite from ground up. The assumption is that the test machine
is a brand new one with only Linux kernel installed. 

The initial test suite inlcudes default compilers/mpis/libs and two necessary applications - Anaconda and Leslie-spec.
Anaconda is used for running the framework, Leslie-spec is our example application to show Contributor to install 
new applications. 

* using Utilities/spack-config to build compilers, MPIs
and libraries, follow the instruction at [SPACK](Utilities/spack-config/README.md)
Contributor can also install those packages by hand by following the way Anaconda is built below, 
In this instruction, we need to use SPACK to install gcc/intel/mvapich2 compilers so that we have necessary tools
to build Leslie-spec. 

* In order to get framework running, two neccessary applications need to be added,
first is Anaconda3:

1. always run boot strap script (The lines start with > are commands)
```
>bash Utilities/spack-config/setup-spack.sh (We assume that this step is already done in SPACK setup above)
>source Utilities/bench_init.sh
>cd Application/Source/anaconda-2019.07/
```
read pace_build_anaconda3.sh
besure to download Anaconda3-2019.07-Linux-x86_64.sh before the next step, and replace the path as needed
then run:
```
>./pace_build_anaconda3.sh
```
This builds anaconda under Application/Install/anaconda3/2019.07
The module file exists already under Application/Module/anaconda3/2019.07.lua

2. next step is to build application e.g. leslie-spec, it is the test application
```
>source Utilities/bench_init.sh
>cd Application/Source/leslie-spec
>./pace_build.sh
```
This builds leslie-spec under Application/Install/leslie-spec/21Feb19

To test whether or not it's installed successfully:
```
>module load leslie-spec/21Feb19
>which les3d.x
```

* Third step is to build database, one should have a mariadb or mysql server uprunning, and use
the script under `utilites/create_provbench_table.mysql` to create database and tables. And this step only 
needs to be done once by Contributor. 
