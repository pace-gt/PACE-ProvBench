
#if [ $EUID != 0 ]; then echo -e "\nRun as root\n"; exit 0; fi

SOFTWARE=vasp
VERSION=5.4.4

#buildall: intel-19.0.3-mvapich2-2.3.1 intel-19.0.3-impi-2019


trap "exit 1" 2

ULP=/usr/local/pace-apps/manual/packages
#ULP=/scratch/PACE-Bench/Applications/install
INSTALL_BASE_DIR=${ULP}/${SOFTWARE}/${VERSION}

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

intel-19.0.3-mvapich2-2.3.1-mri(){
  module purge
  module use /scratch/PACE-Bench/Applications/modules
  module load intel/19.0.3
  module load mvapich2/2.3.1 fftw/3.3.8

  BIN_DEST_NAME="vasp"
  BASEMAKEFILE="pace_intel.mvapich2"
  MAKEFILE="${BASEMAKEFILE}"

  build 2>&1| tee build.${FUNCNAME}.1.out

}


intel-19.0.3-mvapich2-2.3.1(){
  module purge
  module load intel/19.0.3
  module load mvapich2/2.3.1 fftw/3.3.8-mva2 
  export  MPIVERSION=${PACE_FAMILY_MPI}-${PACE_FAMILY_MPI_VERSION}
  export  COMPILERVERSION=${PACE_FAMILY_COMPILER}-${PACE_FAMILY_COMPILER_VERSION}

  BIN_DEST_NAME="vasp"
  BASEMAKEFILE="pace_intel.mvapich2"
  MAKEFILE="${BASEMAKEFILE}"

  build 2>&1| tee build.${FUNCNAME}.1.out

}

intel-19.0.5-mvapich2-2.3.2(){
  module purge
  module load intel/19.0.5
  module load mvapich2/2.3.2 fftw/3.3.8-mva2 
  export  MPIVERSION=${PACE_FAMILY_MPI}-${PACE_FAMILY_MPI_VERSION}
  export  COMPILERVERSION=${PACE_FAMILY_COMPILER}-${PACE_FAMILY_COMPILER_VERSION}

  BIN_DEST_NAME="vasp"
  BASEMAKEFILE="pace_intel.mvapich2"
  MAKEFILE="${BASEMAKEFILE}"

  build 2>&1| tee build.${FUNCNAME}.1.out

}
intel-19.0.3-impi-2019(){
  module purge
  module load intel/19.0.3
  export COMPILERVERSION=intel-19.0.3
  export MPIVERSION=impi-19.0.3

  BIN_DEST_NAME="vasp"
  BASEMAKEFILE="pace_intel.impi"
  MAKEFILE="${BASEMAKEFILE}"  

  build 2>&1| tee build.${FUNCNAME}.1.out
}

if [ $# -eq 0 ]
 then 
  ARGS="intel-19.0.5-mvapich2-2.3.2"

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


