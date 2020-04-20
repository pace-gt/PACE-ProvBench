#!/usr/bin/bash

export BENCH_ROOT=`git rev-parse --show-toplevel`
export BENCH_INSTALL_ROOT=$BENCH_ROOT/Application/Install
#UTIL_MODULE_PATH=$BENCH_ROOT/Application/UtilModule
#export MODULEPATH=$UTIL_MODULE_PATH:$MODULEPATH
#module purge
module load anaconda3/2019.10
export PYTHONPATH=$BENCH_ROOT/SRC:$PYTHONPATH
source activate provbench
if [ ! -f $BENCH_ROOT/Utilities/bench_init.sh ]; then
    ln -s $BENCH_ROOT/Utilities/bench_init_withoutspack.sh $BENCH_ROOT/Utilities/bench_init.sh
fi
