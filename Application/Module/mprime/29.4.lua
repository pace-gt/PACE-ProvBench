--use env BENCH_INSTALL_ROOT which is set at <repo root>/Utilities/bench_init.sh

local base = os.getenv("BENCH_INSTALL_ROOT")
local software = "mprime"
local version = "29.4" 
local path = pathJoin(base, software, version)

help([[
	This LESlie3D benchmark. 
]])

whatis("Sets up environment for "..software.." version "..version)


prepend_path("PATH",path) 

setenv("MPRIMEROOT", path)
