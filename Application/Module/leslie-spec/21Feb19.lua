--use env BENCH_INSTALL_ROOT which is set at <repo root>/Utilities/bench_init.sh

local base = os.getenv("BENCH_INSTALL_ROOT")
local software = "leslie-spec"
local version = "21Feb19" 
local path = pathJoin(base, software, version)

help([[
	This LESlie3D benchmark. 
]])

load("intel/19.0.3/mvapich2/2.3.1")
local compiler = "intel-19.0.3"
local mpi = "mvapich2-2.3.1"
path = pathJoin(path, mpi, compiler)

whatis("Sets up environment for "..software.." version "..version)


prepend_path("PATH",pathJoin(path,"/bin")) 

setenv("LESLIE3DROOT", path)
