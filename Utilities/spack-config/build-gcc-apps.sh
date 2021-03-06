#!/bin/bash
# read BENCH_SPACK_ROOT from a file name bench.spack.root
# set env BENCH_SPACK_ROOT and SPACK_ROOT for etc/spack/config.yaml
./setup-spack.sh
export BENCH_SPACK_ROOT=`cat bench.spack.root`
export SPACK_ROOT=$BENCH_SPACK_ROOT/source
# Step 4:
echo "Step 4: initiate the spack env"
. $SPACK_ROOT/share/spack/setup-env.sh
# Step 5: 
echo "Step 5: build gcc compiler"
spack env activate base_gcc_apps
spack env list
spack install --keep-stage
