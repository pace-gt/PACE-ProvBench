
#if [ $EUID != 0 ]; then echo -e "\nRun as root\n"; exit 0; fi

#The buildall line is for the pace_parallel_build script.
#Use the buildall line with "pace_parallel_build <dirname> all" 
#buildall: generic

BENCH_ROOT=`git rev-parse --show-toplevel`
#Assume MODULEPATH is already been set by init script 
#export MODULEPATH=`cat $BENCH_ROOT/Utilities/spack-config/bench.module.root`
SOFTWARE=anaconda3
VERSION=2019.07

if [ -z $SOFTWARE ]; then echo -e "SOFTWARE variable must be defined"; exit 1; fi
if [ -z $VERSION ]; then echo -e "VERSION variable must be defined"; exit 1; fi

INSTALL_BASE_DIR=${BENCH_ROOT}/Application/Install/${SOFTWARE}/${VERSION}

trap "exit 1" 2

build() {



  module list
  echo "Environment: CC=$CC, CXX=$CXX, CFLAGS=$CFLAGS, FC=$FC, F77=$F77, F90=$F90, FFLAGS=$FFLAGS, MPIVERSION=$MPIVERSION, COMPILERVERSION=$COMPILERVERSION"

  # 1. besure to download Anaconda3-2019.07-Linux-x86_64.sh from https://www.anaconda.com/distribution/ before the next step, and replace the path as needed
  # 2. create a conda cache directory, and put in ~/.condarc,
  # the content of the file is:
  # pkgs_dirs:
  #     - ~/condacache  

  rm -r ${INSTALL_BASE_DIR}
  bash ./Anaconda3-2019.07-Linux-x86_64.sh -b -p ${INSTALL_BASE_DIR}

  echo "Updating anaconda with new licenses..."
  export ORIGPATH=${PATH}
  
  export PATH=${INSTALL_BASE_DIR}/bin:$ORIGPATH
  export PYTHONPATH=${INSTALL_BASE_DIR}/lib/python3.7/site-packages
  echo $PATH
  echo $PYTHONPATH
  conda info --license
  conda update --yes conda
  conda install -c anaconda mysql-connector-python
}

all(){
  BUILDS=$(egrep "^#buildall:" $0 | cut -c 11-)

  for BUILD in $BUILDS
  do
    ($BUILD)
  done

}

generic(){
  #module loads
  #EXTRA_LOADS
  module purge
  # Don't use Haswell optimization if binutils is too old (e.g. RHEL6)

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

