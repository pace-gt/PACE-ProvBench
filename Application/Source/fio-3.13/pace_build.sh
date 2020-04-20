#!/bin/bash
set -x
BENCH_ROOT=`git rev-parse --show-toplevel`
SOFTWARE=fio
VERSION=3.13

trap "exit 1" 2

INSTALL_BASE_DIR=${BENCH_ROOT}/Application/Install/${SOFTWARE}/${VERSION}
build() {

  # Arguments passed to build() should be exportable variables.
  # They should be set by whoever calls build() and should be set with the form "<VARNAME>=arg1 arg2 ...\"

  echo "Environment: CC=$CC, CXX=$CXX, CFLAGS=$CFLAGS, FC=$FC, F77=$F77, F90=$F90, FFLAGS=$FFLAGS, MPIVERSION=$MPIVERSION, COMPILERVERSION=$COMPILERVERSION"
  make clean
  eval ${@} ./configure --prefix=${INSTALL_BASE_DIR} \
    && make -j \
    && make install
    
}

all(){
  BUILDS=$(egrep "^#buildall:" $0 | cut -c 11-)

  for BUILD in $BUILDS
  do
    ($BUILD)
  done

}

intel-19.0.3-mri(){
  module purge
  module load Core/intel-parallel-studio/cluster.2019.3
  export CC=icc
  export CXX=icpc
  export FC=ifort
  export F90=ifort
  export F77=ifort
  export COMPILERVERSION=intel-19.0.3
  build 2>&1| tee build.${FUNCNAME}.1.out
}

if [ $# -eq 0 ]
 then 
  ARGS="intel-19.0.3-mri"

else 
  ARGS=$@
fi

set $ARGS

###################################################################

while [ $# -gt 0 ]
do
  ($1)
  shift
done
