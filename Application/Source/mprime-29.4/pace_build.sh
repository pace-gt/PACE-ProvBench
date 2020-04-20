
#if [ $EUID != 0 ]; then echo -e "\nRun as root\n"; exit 0; fi

#The buildall line is for the pace_parallel_build script.
#Use the buildall line with "pace_parallel_build <dirname> all" 
#buildall: generic
BENCH_ROOT=`git rev-parse --show-toplevel`
SOFTWARE=mprime
VERSION=29.4

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
    mkdir -p $INSTALL_BASE_DIR
    rsync -aup * $INSTALL_BASE_DIR

}
generic(){
	 build  2>&1 | tee build.${FUNCNAME}.1.out

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

