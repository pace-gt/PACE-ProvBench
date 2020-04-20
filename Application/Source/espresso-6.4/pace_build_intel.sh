
#The buildall line is for the pace_parallel_build script.
#Use the buildall line with "pace_parallel_build <dirname> all" 
#buildall: intel-19.0-mvapich2-2.3 

SOFTWARE=espresso
VERSION=6.4

if [ -z $SOFTWARE ]; then echo -e "SOFTWARE variable must be defined"; exit 1; fi
if [ -z $VERSION ]; then echo -e "VERSION variable must be defined"; exit 1; fi

ULP=`git rev-parse --show-toplevel`
TGT=${ULP}/${SOFTWARE}/${VERSION}
mkdir -p $TGT

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
    #eval $d{@} ./configure --prefix=${TGT}/${MPIVERSION}/${COMPILERVERSION} $CONFIGURE_OPTS \
    #Cherry: has to hardcod OPTs and run from command line to make it work 09/06/2018
    #For some reason, `make all -j6` does not work, so manually make all of the packages
    ./configure --prefix=${TGT}/${MPIVERSION}/${COMPILERVERSION}  BLAS_LIBS="-lmkl_gf_lp64 -lmkl_sequential -lmkl_core -lmkl_blacs_intelmpi_lp64 -lpthread"  LAPACK_LIBS="-lmkl_lapack95_lp64 -lmkl_scalapack_lp64" CFLAGS="-O3  -ansi-alias -ip -axCORE-AVX512,CORE-AVX2,AVX,SSE4.2" FFLAGS="-O3  -ansi-alias -ip -axCORE-AVX512,CORE-AVX2,AVX,SSE4.2" \
    && make all -j6 \
    && make install

	rsync -av pseudo ${TGT}/ \
	&& mkdir -pv ${TGT}/${MPIVERSION}/${COMPILERVERSION} \
	&& rsync -avL bin ${TGT}/${MPIVERSION}/${COMPILERVERSION}/ 
    
    
}

all(){
  BUILDS=$(egrep "^#buildall:" $0 | cut -c 11-)

  for BUILD in $BUILDS
  do
    ($BUILD)
  done

}


intel-19.0-mvapich2-2.3(){
  module purge
  module load intel/19.0 mvapich2/2.3
  module load mkl/2019u1
  
  #EXTRA_LOADS

  CONFIGURE_OPTS="--enable-parallel"

  build  2>&1 | tee build.${FUNCNAME}.1.out

}

if [ $# -eq 0 ]
 then 
  ARGS="intel-19.0-mvapich2-2.3"
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

