
#if [ $EUID != 0 ]; then echo -e "\nRun as root\n"; exit 0; fi

#The buildall line is for the pace_parallel_build script.
#Use the buildall line with "pace_parallel_build <dirname> all" 
#buildall: intel-19.0-mvapich2-2.3

BENCH_ROOT=`git rev-parse --show-toplevel`
export MODULEPATH=`cat $BENCH_ROOT/Utilities/spack-config/bench.module.root`

SOFTWARE=lammps
VERSION=08Aug19

if [ -z $SOFTWARE ]; then echo -e "SOFTWARE variable must be defined"; exit 1; fi
if [ -z $VERSION ]; then echo -e "VERSION variable must be defined"; exit 1; fi

# the following line is required, it gives the root path for application binaries.
INSTALL_BASE_DIR=${BENCH_ROOT}/Application/Install/${SOFTWARE}/${VERSION}
trap "exit 1" 2

build() {

    #Arguments passed to build() should be exportable variables.
    #They should be set by whoever calls build() and should be set with the form "<VARNAME>=arg1 arg2 ...\"
    module list
    echo "Environment: CC=$CC, CXX=$CXX, CFLAGS=$CFLAGS, FC=$FC, F77=$F77, F90=$F90, FFLAGS=$FFLAGS, MPIVERSION=$MPIVERSION, COMPILERVERSION=$COMPILERVERSION"

    rm -rf build
    mkdir build;cd build
    cmake ../cmake \
	-DCMAKE_INSTALL_PREFIX=${INSTALL_BASE_DIR}/${MPIVERSION}/${COMPILERVERSION} \
	-DBUILD_MPI=yes \
	-DCMAKE_BUILD_TYPE=Release \
    -DCMAKE_CXX_FLAGS="-O3  -ansi-alias -ip -axCORE-AVX512,CORE-AVX2,AVX,SSE4.2 -mcmodel medium" \
    -DCMAKE_C_FLAGS="-O3  -ansi-alias -ip -axCORE-AVX512,CORE-AVX2,AVX,SSE4.2 -mcmodel medium" \
    -DCMAKE_Fortran_FLAGS="-O3  -ansi-alias -ip -axCORE-AVX512,CORE-AVX2,AVX,SSE4.2 -mcmodel medium" \
	-DBUILD_EXE=yes \
	-DBUILD_LIB=yes \
	-DBUILD_SHARD_LIBS=yes \
	-DFFT=FFTW3 \
	-DFFTW3_INCLUDE_DIR=${FFTWROOT}/include \
	-DFFTW3_LIBRARY=${FFTWROOT}/lib/libfftw3.so\
	-DPKG_ASPHERE=yes \
	-DPKG_REAX=yes \
	-DPKG_MEAM=yes \
	-DPKG_POEMS=yes \
	-DPKG_MANYBODY=yes \
	-DPKG_REPLICA=yes \
	-DPKG_CLASS2=yes \
	-DPKG_MC=yes \
	-DPKG_RIGID=yes \
	-DPKG_PERI=yes \
	-DPKG_DIPOLE=yes \
	-DPKG_USER-EFF=yes \
	-DPKG_USER-DPD=yes \
	-DPKG_USER-MESO=yes \
	-DPKG_USER-SPH=yes \
	-DPKG_USER-AWPWD=yes \
	-DPKG_SPIN=yes \
	-DPKG_QEQ=yes \
	-DPKG_MISC=yes \
	-DPKG_COLLOID=yes \
	-DPKG_GRANULAR=yes \
	-DPKG_USER-MISC=yes \
	-DPKG_MOLECULE=yes \
	-DPKG_USER-PHONON=yes \
	-DPKG_KSPACE=yes \
	-DPKG_USER-REAXC=yes 
	

    make yes-all
    make yes-user
    make  
    make install

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
  module load intel/19.0.3/mvapich2/2.3.1
  # the following two lines set up the subdirectory for binaries, so that one can easily tell
  # what compiler and mpi are used.
  MPIVERSION="mvapich2-2.3.1"
  COMPILERVERSION="intel-19.0.3"
  module load cmake/3.15.1
  module load fftw/3.3.8


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

