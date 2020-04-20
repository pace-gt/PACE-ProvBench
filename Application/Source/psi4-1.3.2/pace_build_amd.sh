
#if [ $EUID != 0 ]; then echo -e "\nRun as root\n"; exit 0; fi
#buildall: generic

BENCH_ROOT=`git rev-parse --show-toplevel`

SOFTWARE=psi4
VERSION=1.3.2



trap "exit 1" 2

# the following line is required, it gives the root path for application binaries.
INSTALL_BASE_DIR=${BENCH_ROOT}/Application/Install/${SOFTWARE}/${VERSION}

build() {

    #Arguments passed to build() should be exportable variables.
    #They should be set by whoever calls build() and should be set with the form "<VARNAME>=arg1 arg2 ...\"
    module list
    echo "Environment: CC=$CC, CXX=$CXX, CFLAGS=$CFLAGS, FC=$FC, F77=$F77, F90=$F90, FFLAGS=$FFLAGS, MPIVERSION=$MPIVERSION, COMPILERVERSION=$COMPILERVERSION"

    
    rm -rf build
    mkdir build

	cmake -H. -Bbuild\
    -DCMAKE_INSTALL_PREFIX=${INSTALL_BASE_DIR}/${MPIVERSION}/${COMPILERVERSION}\
	-DPYMOD_INSTALL_LIBDIR=${INSTALL_BASE_DIR}/${MPIVERSION}/${COMPILERVERSION}/lib \
    -DCMAKE_C_COMPILER=icc \
	-DCMAKE_C_FLAGS="-O3 -xhost" \
    -DCMAKE_CXX_COMPILER=icpc \
	-DCMAKE_CXX_FLAGS="-O3 -xhost" \
    -DCMAKE_Fortran_COMPILER=ifort \
	-DCMAKE_Fortran_FLAGS="-O3 -xhost" \
    -DPYTHON_LIBRARY=$PYTHONROOT/lib/libpython3.7m.so \
    -DPYTHON_INCLUDE_DIR=$PYTHONROOT/include 
    #-DCMAKE_C_COMPILER=icc \
	#-DCMAKE_C_FLAGS="-O3 -qopt-report=5 -march=core-avx2 -fma -ftz -fomit-frame-pointer -ipo -no-prec-div -fp-model fast=2" \
    #-DCMAKE_CXX_COMPILER=icpc \
	#-DCMAKE_CXX_FLAGS="-O3 -qopt-report=5 -march=core-avx2 -fma -ftz -fomit-frame-pointer -ipo -no-prec-div -fp-model fast=2" \
    #-DCMAKE_Fortran_COMPILER=ifort \
	#-DCMAKE_Fortran_FLAGS="-O3 -qopt-report=5 -march=core-avx2 -fma -ftz -fomit-frame-pointer -ipo -no-prec-div -fp-model fast=2" \
    #-DLAPACK_LIBRARIES=$MKLROOT/lib/intel64/libmkl_intel_lp64.a;$MKLROOT/lib/intel64/libmkl_intel_thread.a \
    #-DLAPACK_INCLUDE_DIRS=$MKLROOT/include 

    cd build
    make -j8
    make install
	#build pint locally
	conda create --prefix ${INSTALL_BASE_DIR}/${MPIVERSION}/${COMPILERVERSION}/python-3.7  -c psi4 pint
	source activate ${INSTALL_BASE_DIR}/${MPIVERSION}/${COMPILERVERSION}/python-3.7
	conda install deepdiff -c conda-forge
}

all(){
  BUILDS=$(egrep "^#buildall:" $0 | cut -c 11-)

  for BUILD in $BUILDS
  do
    ($BUILD)
  done

}


generic(){

  source ${BENCH_ROOT}/Utilities/bench_init.sh
  module purge
  module load Core/intel-parallel-studio/cluster.2019.3
  export CC=icc
  export CXX=icpc
  export FC=ifort
  export F77=ifort
  COMPILERVERSION="intel-19.0.3"
  module load Core/cmake/3.15.1
  module load anaconda3/2019.07
  module load Core/gcc/8.3.0

  build 2>&1 | tee build.${FUNCNAME}.1.out

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


