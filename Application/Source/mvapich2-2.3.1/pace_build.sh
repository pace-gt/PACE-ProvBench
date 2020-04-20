#The buildall line is for the pace_parallel_build script.
#Use the buildall line with "pace_parallel_build <dirname> all" 


#buildall:generic

BENCH_ROOT=`git rev-parse --show-toplevel`
#FL: Don't need the following line if not dependnt on the spack build libs
#export MODULEPATH=`cat $BENCH_ROOT/Utilities/spack-config/bench.module.root`


SOFTWARE=mvapich2
VERSION=2.3.1

if [ -z $SOFTWARE ]; then echo -e "SOFTWARE variable must be defined"; exit 1; fi
if [ -z $VERSION ]; then echo -e "VERSION variable must be defined"; exit 1; fi

trap "exit 1" 2
# the following line is required, it gives the root path for application binaries.

INSTALL_BASE_DIR=${BENCH_ROOT}/Application/Install/${SOFTWARE}/${VERSION}


build() {

    #Arguments passed to build() should be exportable variables.
    #They should be set by whoever calls build() and should be set with the form "<VARNAME>=arg1 arg2 ...\"
    module list
    unset F90 F90FLAGS
   # autoreconf
    echo "Environment: CC=$CC, CXX=$CXX, CFLAGS=$CFLAGS, FC=$FC, F77=$F77, F90=$F90, FFLAGS=$FFLAGS, MPIVERSION=$MPIVERSION, COMPILERVERSION=$COMPILERVERSION"

    make clean
    #The CC, FC, ... environment variables should already be set.
    #Additionally, MPIVERSION and COMPILERVERSION should be set by the loaded mpi or compiler module.
    #The "eval ..." statement lets the user pass variables into build that get passed to the maker or configurer
    eval ${@} ./configure --prefix=$INSTALL_BASE_DIR/$COMPILERVERSION/ $CONFIGURE_OPTS \
    && make -j8 \
    && make install


}

#CONFIGURE_OPTS="--with-hwloc --with-pbs=/opt/torque/current --enable-romio --with-file-system=ufs+nfs --enable-shared --enable-sharedlibs=gcc "
CONFIGURE_OPTS="--with-hwloc --with-pbs=/opt/torque/current --with-file-system=ufs+nfs --enable-shared --enable-sharedlibs=gcc --with-ch3-rank-bits=32"
#CONFIGURE_OPTS="--enable-romio --with-file-system=ufs+nfs --enable-shared --enable-sharedlibs=gcc "

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
  module load aocc/2.1.0
  export CC=clang
  export CXX=clang++
  export FC=flang
  export F77=flang
  export F90=flang
  export COMPILERVERSION=aocc-2.1.0

  build "MPICH2LIB_CFLAGS=\"-O2 g \"" "MPICH2LIB_FFLAGS=-O2" "MPICH2LIB_CXXFLAGS=-O2" 2>&1 | tee build.${FUNCNAME}.out

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
  module purge
  ($1)
  shift
done


