#!/bin/bash
# Written by Fang (Cherry) Liu at 10/24/2019 
# setup-spack.sh is run by Contributor to get ready for spack environment and build benchmark suite
# it checks out SPACK repo and propagates the spack configuration
# args:
# $1 specifies where to check out spack from command line
#    or default location is used which is under <bench repo root>/Application/Spack
# output:
# a file name bench.spack.root -> this file is used by the following steps to find bench spack dir, 
# including sub dirs for packages, modules and source
# a file name bench.module.root -> poin to the spack generated module file, used by manual build packages's 
# pace_build.sh to set up MODULEPATH during the compilation time


echo $#

if [[ $# -eq 1 ]]; then 
    echo "Use user specified the BENCH SPACK root directory"
    export BENCH_SPACK_ROOT=$1
else
    echo "Use system default BENCH SPACK root directory"
    export BENCH_SPACK_ROOT=`git rev-parse --show-toplevel`/Application/Spack
fi


# Step 1: check out the repo
echo "Step 1: check out the repo, if the repo exists already, nothing is done"
export SPACK_ROOT=$BENCH_SPACK_ROOT/source
echo $BENCH_SPACK_ROOT >bench.spack.root
echo $BENCH_SPACK_ROOT/modules/lmod/linux-rhel7-x86_64 >bench.module.root

if [ ! -d "$SPACK_ROOT" ] ; then
    git clone https://github.com/spack/spack -b v0.13.0 $BENCH_SPACK_ROOT/source
else
    echo "spack repo exists! If you want to checkout a new version, please do rm -rf $BENCH_SPACK_ROOT/source"    
fi

# Step 2: 
echo "Step 2: copy the config to newly checked out source"
cp -r etc/spack/* $SPACK_ROOT/etc/spack/ 
cp -r  var/spack/environments $SPACK_ROOT/var/spack/ 

# Step 3:
echo "Step 3: create directory for binaries and modules"
if [ ! -d "$BENCH_SPACK_ROOT/packages" ]; then
    mkdir -p $BENCH_SPACK_ROOT/packages
fi

if [ ! -d "$BENCH_SPACK_ROOT/modules/lmod" ]; then
    mkdir -p $BENCH_SPACK_ROOT/modules/lmod
fi

