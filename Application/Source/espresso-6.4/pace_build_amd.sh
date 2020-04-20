
#The buildall line is for the pace_parallel_build script.
#Use the buildall line with "pace_parallel_build <dirname> all" 
#buildall: generic 

BENCH_ROOT=`git rev-parse --show-toplevel`

SOFTWARE=espresso
VERSION=6.4

if [ -z $SOFTWARE ]; then echo -e "SOFTWARE variable must be defined"; exit 1; fi
if [ -z $VERSION ]; then echo -e "VERSION variable must be defined"; exit 1; fi


INSTALL_BASE_DIR=${BENCH_ROOT}/Application/Install/${SOFTWARE}/${VERSION}

trap "exit 1" 2

build() {
    #Arguments passed to build() should be exportable variables.
    #They should be set by whoever calls build() and should be set with the form "<VARNAME>=arg1 arg2 ...\"
    module list
    echo "Environment: CC=$CC, CXX=$CXX, CFLAGS=$CFLAGS, FC=$FC, F77=$F77, F90=$F90, FFLAGS=$FFLAGS, MPIVERSION=$MPIVERSION, COMPILERVERSION=$COMPILERVERSION"

    #The CC, FC, ... environment variables should already be set.
    #Additionally, MPIVERSION and COMPILERVERSION should be set by the loaded mpi or compiler module.
    #The "eval ..." statement lets the user pass variables into build that get passed to the maker or configurer
    make clean
    make distclean
    find . -name "*.mod" -exec rm -fv {} \;
    #Cherry: has to hardcod OPTs and run from command line to make it work 09/06/2018
    mkdir -p ${INSTALL_BASE_DIR}
    ./configure --prefix=${INSTALL_BASE_DIR}/${MPIVERSION}/${COMPILERVERSION}  BLAS_LIBS="-lmkl_gf_lp64 -lmkl_sequential -lmkl_core -lmkl_blacs_intelmpi_lp64 -lpthread"  LAPACK_LIBS="-lmkl_lapack95_lp64 -lmkl_scalapack_lp64" CFLAGS="-O3 -qopt-report=5 -march=core-avx2 -fma -ftz -fomit-frame-pointer -ipo -no-prec-div -fp-model fast=2" FFLAGS="-O3 -qopt-report=5 -march=core-avx2 -fma -ftz -fomit-frame-pointer -ipo -no-prec-div -fp-model fast=2" \
    && make all -j6 \
    && make install

	rsync -av pseudo ${INSTALL_BASE_DIR}/ \
	&& mkdir -pv ${INSTALL_BASE_DIR}/${MPIVERSION}/${COMPILERVERSION} \
	&& rsync -avL bin ${INSTALL_BASE_DIR}/${MPIVERSION}/${COMPILERVERSION}/ 
    
    
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
  export CC=mpicc
  export CXX=mpic++
  export FC=mpif90
  export F77=mpif77

  MPIVERSION="mvapich2-2.3.1"
  COMPILERVERSION="intel-19.0.3"

  #EXTRA_LOADS

  CONFIGURE_OPTS="--enable-parallel"

  build  2>&1 | tee build.${FUNCNAME}.1.out

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

