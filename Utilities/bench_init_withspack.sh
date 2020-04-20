#!/usr/bin/bash

export BENCH_ROOT=`git rev-parse --show-toplevel`
SPACK_MODULE_PATH=`cat $BENCH_ROOT/Utilities/spack-config/bench.module.root`
MANUAL_MODULE_PATH=$BENCH_ROOT/Application/Module
UTIL_MODULE_PATH=$BENCH_ROOT/Application/UtilModule
export BENCH_SPACK_ROOT=`cat $BENCH_ROOT/Utilities/spack-config/bench.spack.root`
export SPACK_ROOT=$BENCH_SPACK_ROOT/source
. $SPACK_ROOT/share/spack/setup-env.sh
#export MODULEPATH=$SPACK_MODULE_PATH:$MANUAL_MODULE_PATH:$MODULEPATH
export MODULEPATH=$UTIL_MODULE_PATH:$SPACK_MODULE_PATH:$MANUAL_MODULE_PATH
export BENCH_INSTALL_ROOT=$BENCH_ROOT/Application/Install
module purge
module load anaconda3/2019.07
export PYTHONPATH=$BENCH_ROOT/SRC:$PYTHONPATH
if [ ! -f $BENCH_ROOT/Utilities/bench_init.sh ]; then
    ln -s $BENCH_ROOT/Utilities/bench_init_withspack.sh $BENCH_ROOT/Utilities/bench_init.sh 
fi
