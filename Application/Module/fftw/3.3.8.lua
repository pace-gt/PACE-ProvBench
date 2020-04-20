local base = "/gpfs/pace1/project/pace-admins/Benchmarks/CascadeLake/Install"
local software = "fftw"
local version = "3.3.8"
local path = pathJoin(base, software, version)

help([[
  This module sets up fftw 3.3.8. 
]])


load("intel/19.0")
load("mvapich2/2.3")
local compiler = os.getenv("COMPILERVERSION")
local mpi = os.getenv("MPIVERSION")
path = pathJoin(path, mpi, compiler)

whatis("Sets up environment for "..software.." version "..version)

append_path("LD_LIBRARY_PATH", pathJoin(path, "/lib"))
append_path("LD_RUN_PATH", pathJoin(path, "/lib"))
append_path("LDFLAGS", "-L"..pathJoin(path,  "/lib"), " ")
append_path("CPPFLAGS", "-I"..pathJoin(path,  "/include"), " ")
append_path("C_INCLUDE_PATH", "-I"..pathJoin(path,  "/include"), " ")
append_path("CPLUS_INCLUDE_PATH", "-I"..pathJoin(path,  "/include"), " ")

setenv("FFTWROOT", path)
