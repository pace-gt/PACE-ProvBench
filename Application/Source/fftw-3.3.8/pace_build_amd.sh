#if [ $EUID != 0 ]; then echo -e "\nRun as root\n"; exit 0; fi

#buildall: generic 
BENCH_ROOT=`git rev-parse --show-toplevel`


SOFTWARE=fftw
VERSION=3.3.8

# the following line is required, it gives the root path for application binaries.
INSTALL_BASE_DIR=${BENCH_ROOT}/Application/Install/${SOFTWARE}/${VERSION}
trap "exit 1" 2

build() {

    #Arguments passed to build() should be exportable variables.
    #They should be set by whoever calls build() and should be set with the form "<VARNAME>=arg1 arg2 ...\"
    module list
    echo "Environment: CC=$CC, CXX=$CXX, CFLAGS=$CFLAGS, FC=$FC, F77=$F77, F90=$F90, FFLAGS=$FFLAGS, MPIVERSION=$MPIVERSION, COMPILERVERSION=$COMPILERVERSION"

    make clean
    #The CC, FC, ... environment variables should already be set.
    #Additionally, MPIVERSION and COMPILERVERSION should be set by the loaded mpi or compiler module.
    #The "eval ..." statement lets the user pass variables into build that get passed to the maker or configurer
    eval ${@} ./configure --prefix=$INSTALL_BASE_DIR/$MPIVERSION/$COMPILERVERSION $CONFIGURE_OPTS \
    && make -j 8 \
    && make install

}

CONFIGURE_SERIAL='--disable-doc --enable-openmp --enable-threads --enable-sse2 --enable-shared'
CONFIGURE_SERIAL_FLOAT='--disable-doc --enable-openmp --enable-threads --enable-sse2 --enable-shared --enable-single'
CONFIGURE_SERIAL_LONGDOUBLE=' --disable-doc  --enable-openmp --enable-threads --enable-shared --enable-long-double'

CONFIGURE_MPI=' --disable-doc --enable-mpi --enable-openmp --enable-threads --enable-sse2 --enable-shared'
CONFIGURE_MPI_FLOAT=' --disable-doc --enable-mpi --enable-openmp --enable-threads --enable-sse2 --enable-shared --enable-single'
CONFIGURE_MPI_LONGDOUBLE=' --disable-doc --enable-mpi --enable-openmp --enable-threads --enable-shared --enable-long-double'

CAFFLAG=' -O3  -ansi-alias -ip -axCORE-AVX512,CORE-AVX2,AVX,SSE4.2  -fomit-frame-pointer -malign-double -ffast-math '

generic(){
  module purge
  module load intel/19.0.3/mvapich2/2.3.1
  export CC=mpicc
  export CXX=mpic++
  export FC=mpif90
  export F90=mpif90
  export F77=mpif77
  MPIVERSION="mvapich2-2.3.1"
  COMPILERVERSION="intel-19.0.3"
  modlue list
  #EXTRA_LOADS

  CONFIGURE_OPTS=$CONFIGURE_MPI
  build "CFLAGS=\"-O3 -qopt-report=5 -march=core-avx2 -fma -ftz -fomit-frame-pointer -ipo -no-prec-div -fp-model fast=2\"" "FFLAGS=\"-O3 -qopt-report=5 -march=core-avx2 -fma -ftz -fomit-frame-pointer -ipo -no-prec-div -fp-model fast=2\"" 2>&1 | tee -a build.${FUNCNAME}.1.out

  CONFIGURE_OPTS=$CONFIGURE_MPI_FLOAT
  build "CFLAGS=\"-O3 -qopt-report=5 -march=core-avx2 -fma -ftz -fomit-frame-pointer -ipo -no-prec-div -fp-model fast=2\"" "FFLAGS=\"-O3 -qopt-report=5 -march=core-avx2 -fma -ftz -fomit-frame-pointer -ipo -no-prec-div -fp-model fast=2\"" 2>&1 | tee -a build.${FUNCNAME}.1.out

  #CONFIGURE_OPTS=$CONFIGURE_MPI_LONGDOUBLE
  #build "CFLAGS=\"-O3 -qopt-report=5 -march=core-avx2 -fma -ftz -fomit-frame-pointer -ipo -no-prec-div -fp-model fast=2\"" "FFLAGS=\"-O3 -qopt-report=5 -march=core-avx2 -fma -ftz -fomit-frame-pointer -ipo -no-prec-div -fp-model fast=2\"" 2>&1 | tee -a build.${FUNCNAME}.1.out
}



all(){
  BUILDS=$(egrep "^#buildall:" $0 | cut -c 11-)

  for BUILD in $BUILDS
  do
    ($BUILD)
  done

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



