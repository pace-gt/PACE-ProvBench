
#if [ $EUID != 0 ]; then echo -e "\nRun as root\n"; exit 0; fi

SOFTWARE=psi4
VERSION=1.3.2

#buildall: intel-19.0.3


trap "exit 1" 2

ULP=/usr/local/pace-apps/manual/packages
#ULP=/scratch/PACE-Bench/Applications/install

INSTALL_BASE_DIR=${ULP}/${SOFTWARE}/${VERSION}

build() {

    #Arguments passed to build() should be exportable variables.
    #They should be set by whoever calls build() and should be set with the form "<VARNAME>=arg1 arg2 ...\"
    module list
    COMPILERVERSION=${PACE_FAMILY_COMPILER}-${PACE_FAMILY_COMPILER_VERSION}
    echo "Environment: CC=$CC, CXX=$CXX, CFLAGS=$CFLAGS, FC=$FC, F77=$F77, F90=$F90, FFLAGS=$FFLAGS, MPIVERSION=$MPIVERSION, COMPILERVERSION=$COMPILERVERSION"

    
    rm -rf build
    mkdir build

	cmake -H. -Bbuild\
    -DCMAKE_INSTALL_PREFIX=${INSTALL_BASE_DIR}/${MPIVERSION}/${COMPILERVERSION}\
	-DPYMOD_INSTALL_LIBDIR=${INSTALL_BASE_DIR}/${MPIVERSION}/${COMPILERVERSION}/lib \
    -DCMAKE_C_COMPILER=icc \
	-DCMAKE_C_FLAGS="-ansi-alias -ip -axCORE-AVX512,CORE-AVX2,AVX,SSE4.2" \
    -DCMAKE_CXX_COMPILER=icpc \
	-DCMAKE_CXX_FLAGS="-ansi-alias -ip -axCORE-AVX512,CORE-AVX2,AVX,SSE4.2" \
    -DCMAKE_Fortran_COMPILER=ifort \
	-DCMAKE_Fortran_FLAGS="-ansi-alias -ip -axCORE-AVX512,CORE-AVX2,AVX,SSE4.2" \
    -DPYTHON_LIBRARY=$PYTHONROOT/lib/libpython3.7m.so \
    -DPYTHON_INCLUDE_DIR=$PYTHONROOT/include 
    #-DLAPACK_LIBRARIES=$MKLROOT/lib/intel64/libmkl_intel_lp64.a;$MKLROOT/lib/intel64/libmkl_intel_thread.a \
    #-DLAPACK_INCLUDE_DIRS=$MKLROOT/include 

    cd build
    make -j8
    make install
	#build pint locally
	conda create --prefix ${INSTALL_BASE_DIR}/${MPIVERSION}/${COMPILERVERSION}/python-3.7  -c psi4 pint
	source activate ${INSTALL_BASE_DIR}/${MPIVERSION}/${COMPILERVERSION}/python-3.7
	conda install deepdiff -c conda-forge
        conda install pydantic -c conda-forge
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
#  module use /scratch/PACE-Bench/Applications/modules
  module load intel/19.0.3 
  module load cmake/3.15.1
  module load gcc-compatibility/8.3.0
  module load anaconda3/2019.07 #EXTRA_LOADS

  build 2>&1 | tee build.${FUNCNAME}.1.out

}
intel-19.0.3(){

  module purge
  module load intel/19.0.3 
  export COMPILERVERSION=intel-19.0.3
  module load cmake/3.15.1
  module load gcc-compatibility/8.3.0

  module load anaconda3/2019.07 #EXTRA_LOADS

  build 2>&1 | tee build.${FUNCNAME}.1.out

}
if [ $# -eq 0 ]
 then 
  ARGS="intel-19.0.3"

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


