
#if [ $EUID != 0 ]; then echo -e "\nRun as root\n"; exit 0; fi

BENCH_ROOT=`git rev-parse --show-toplevel`
SOFTWARE=vasp
VERSION=5.4.4

#buildall: generic 


trap "exit 1" 2

INSTALL_BASE_DIR=${BENCH_ROOT}/Application/Install/${SOFTWARE}/${VERSION}

build() {

    #Arguments passed to build() should be exportable variables.
    #They should be set by whoever calls build() and should be set with the form "<VARNAME>=arg1 arg2 ...\"
    module list
    echo "Environment: CC=$CC, CXX=$CXX, CFLAGS=$CFLAGS, FC=$FC, F77=$F77, F90=$F90, FFLAGS=$FFLAGS, MPIVERSION=$MPIVERSION, COMPILERVERSION=$COMPILERVERSION"

    
    cd vasp.5.4.4
    rm -rf build
    mkdir build
    cp makefile.$MAKEFILE makefile.include
    make clean
    #rm vasp
    make std
    make gam
    make ncl 
    #make gpu
    #make gpu_ncl


    INSTALL=$INSTALL_BASE_DIR/$MPIVERSION/$COMPILERVERSION/bin
    mkdir -p $INSTALL
    cp -v build/std/vasp $INSTALL/vasp     
    cp -v build/gam/vasp $INSTALL/vasp_gamma     
    cp -v build/ncl/vasp $INSTALL/vasp_noncollinear     

    cd ../
    cp -v data $INSTALL/../
}

all(){
  BUILDS=$(egrep "^#buildall:" $0 | cut -c 11-)

  for BUILD in $BUILDS
  do
    ($BUILD)
  done

}

generic(){
  module purge
  module load Core/intel-parallel-studio/cluster.2019.3
  module load intel/19.0.3/mvapich2/2.3.1
  module load fftw/3.3.8
  export CC=mpicc
  export CXX=mpic++
  export FC=mpif90
  export F90=mpif90
  export F77=mpif77
  # the following two lines set up the subdirectory for binaries, so that one can easily tell
  # what compiler and mpi are used.
  MPIVERSION="mvapich2-2.3.1"
  COMPILERVERSION="intel-19.0.3"

  BIN_DEST_NAME="vasp"
  BASEMAKEFILE="pace_amd.mvapich2"
  MAKEFILE="${BASEMAKEFILE}"

  build 2>&1| tee build.${FUNCNAME}.1.out

}

if [ $# -eq 0 ]
 then 
  ARGS="generic"

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


