--use env BENCH_INSTALL_ROOT which is set at <repo root>/Utilities/bench_init.sh

local base = os.getenv("BENCH_INSTALL_ROOT")
local software = "namd"
local version = "2.13" 
local path = pathJoin(base, software, version)

help([[
	This NAMD benchmark. 
]])

whatis("Sets up environment for "..software.." version "..version)


prepend_path("PATH",path)


setenv("NAMDROOT", path)
