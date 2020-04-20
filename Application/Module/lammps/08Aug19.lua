local base = "/gpfs/pace1/project/pace-admins/Benchmarks/CascadeLake/Install"
local software = "lammps"
local version = "22Aug18" 
local path = pathJoin(base, software, version)

help([[
  This module sets up lammps 22Aug18, code is downloaded at https://lammps.sandia.gov/

  Basic Usage:
  0) Copy over the example first:
    cp -r $LAMMPSROOT/example .
  1) Modify the pbs script with corresponding queue and resources 
    cd example
    qsub lammps.pbs
]])

prereq_any("intel/19.0")
prereq("mvapich2/2.3")
prereq("fftw/3.3.8")
local compiler = os.getenv("COMPILERVERSION")
local mpi = os.getenv("MPIVERSION")
path = pathJoin(path, mpi, compiler)

whatis("Sets up environment for "..software.." version "..version)


prepend_path("PATH",pathJoin(path,"/bin")) 
prepend_path("LD_LIBRARY_PATH", pathJoin(path, "/lib"))
prepend_path("LDFLAGS", "-L"..pathJoin(path,  "/lib"), " ")
prepend_path("CPPFLAGS", "-I"..pathJoin(path,  "/include"), " ")

setenv("LAMMPSROOT", path)
