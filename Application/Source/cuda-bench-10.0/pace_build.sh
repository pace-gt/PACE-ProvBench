
#if [ $EUID != 0 ]; then echo -e "\nRun as root\n"; exit 0; fi

SOFTWARE=cuda-bench
VERSION=10.0

#buildall: intel-19.0-mvapich2-2.3

#Cherry 4/5/2019
#Three test cases from cuda/10.0 samples folder
#0_Simple/matrixMulCUBLAS
#4_Finance/BlackScholes
#4_Finance/quasirandomGenerator

trap "exit 1" 2

ULP=/gpfs/pace1/project/pace-admins/Benchmarks/CascadeLake/Install
INSTALL_BASE_DIR=${ULP}/${SOFTWARE}/${VERSION}

build() {

    #Arguments passed to build() should be exportable variables.
    #They should be set by whoever calls build() and should be set with the form "<VARNAME>=arg1 arg2 ...\"
    module list
    echo "Environment: CC=$CC, CXX=$CXX, CFLAGS=$CFLAGS, FC=$FC, F77=$F77, F90=$F90, FFLAGS=$FFLAGS, MPIVERSION=$MPIVERSION, COMPILERVERSION=$COMPILERVERSION"
    cd matrixMulCUBLAS
	make -e PREFIX=$INSTALL_BASE_DIR
    cd ../BlackScholes
	make -e PREFIX=$INSTALL_BASE_DIR
    cd ../quasirandomGenerator
	make -e PREFIX=$INSTALL_BASE_DIR
    cd ../
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
	module load cuda/10.0
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


