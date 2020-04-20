local base = "/gpfs/pace1/project/pace-admins/Benchmarks/CascadeLake/Install"
local software = "cuda-bench"
local version = "10.0"
local path = pathJoin(base, software, version)

help([[
  This module sets up cuda-bench 10.0
	1) First do:
		module load cuda-bench/10.0 
	2) Next, use the command:
		matrixMulCUBLAS --sizemult=1000
		BlackScholes
		quasirandomGenerator
]])

load("cuda/10.0")

whatis("Sets up environment for "..software.." version "..version)

prepend_path("PATH", pathJoin(path, "/bin"))
setenv("CUDABENCHROOT", path)
