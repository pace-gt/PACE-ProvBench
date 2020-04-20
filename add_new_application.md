# Add A New Application to Test Suite

There are two cases can follow the steps below to be added to the test suite:

* Manually adding Compilers/MPIs/Libs/Apps , e.g. Anaconda, Matlab, etc.
* Adding Applications based on top of SPACK built compiler and libraries, e.g. Leslie-spec, Lammps

### Build Templates
For installation needs to be compiled, use leslie-spec's build script
`Application/Source/leslie-spec/pace_build.sh`
as template. 

For installation only binary installed, use Anaconda's build script
`Application/Source/anaconda-2019.07/pace_build_anaconda3.sh`
as a template. 

Note: one always makes sure to add the following two lines at the beginning of `pace_build.sh`:
```
BENCH_ROOT=`git rev-parse --show-toplevel`
export MODULEPATH=`cat $BENCH_ROOT/Utilities/spack-config/bench.module.root`
```
these two lines define the modulepath from where spack build libraries, so that all compilers' and mpis' module
can be loaded to build the given application. 

The line 
`INSTALL_BASE_DIR=${BENCH_ROOT}/Application/Install/${SOFTWARE}/${VERSION}`
defines the base installation location for the software,
actual installation location can be:
`INSTALL=${INSTALL_BASE_DIR}/${MPIVERSION}/${COMPILERVERSION}`
where 
```
    MPIVERSION="mvapich2-2.3.2"
    COMPILERVERSION="intel-19.0.3"
```
are defined in the `pace_build.sh` for dependent compilers. 

### New Application

For each new application, one needs to create a folder for the source code `Application/Source/<app name>`.
Source code can be downloaded at the same place, not necessary to put in git.
Copying the template `pace_build.sh` to the application source directory. 
One needs to modify `pace_build.sh` to follow the build the instruction for given software. 

### Module Template

One can use module template from `Application/Module/leslie-spec/21Feb19.lua`, 
one should ensure the base location is set as:
`local base = os.getenv("BENCH_INSTALL_ROOT")`
where `BENCH_INSTALL_ROOT` is set by `source <repo root>/Utilities/bench_init.sh`

### New Module file
After building the software, module file needs to be added to `Application/Module/<app name>/<version>.lua`. 

### Recipe Template
Recipe template is `Application/Recipe/leslie-spec.inp`.
Recipe file defines the application name and version usually in the format `<app name>/<version>` e.g. `leslie-spec/21Feb19`
is the same as the module structure we defined earlier.
It also defines how to run the application from command line, given the setting as:
```
[Application]    
		appname=leslie-spec    -> required
		version=21Feb19        -> required
[Command]   
    launcher=mpirun            -> optional for non parallel job
    launcherargs=-np $NPROCS -machinefile $MACHINEFILE -> optional for non parallel job
    exe=les3d.x                -> required
    exeargs=                   -> optional
    outputfile=time.output     -> required
```
The above configuration will generate the command as follows:
`(time mpirun -np $NPROCS -machinefile $MACHINEFILE les3d.x ) 2>&1| tee -a time.output`


### Add new Recipe
The third step for adding a new software is to add recipe to `Application/Recipe/<app name>.inp`, 
recipe needs to be named as `<app name>.inp`, and <app name> needs to be consistent from all the place it appears. 
    

### New Application Input file
New application also needs to provide the input files, and put under `Application/Input/<app name>`, 
the generated script will copy the input file from given <app name> to test directory. 
If input file name needs to be explicitly added to the command, be sure to add it to the above
`Command` block's exeargs field. 
