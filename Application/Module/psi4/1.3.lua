local base = "/gpfs/pace1/project/pace-admins/Benchmarks/CascadeLake/Install"
local software = "psi4"
local version = "1.3"
local path = pathJoin(base, software, version)

help([[
  This module sets up psi3 version 1.3 
  only licensed users can use this software, vasp is a modeling software used for molecular analysis, mainly used by researchers

	1) First do:
		module load psi3/1.3
	2) Next, use the command:
		psi4 input-file-of-any-name-or-extension -n<#cores>
]])

load("intel/19.0", "mkl/2019u1","anaconda3/5.1.0")

whatis("Sets up environment for "..software.." version "..version)

local compiler = os.getenv("COMPILERVERSION")
local mpi = os.getenv("MPIVERSION")
path = pathJoin(path, mpi, compiler)
prepend_path("PATH", pathJoin(path, "/bin"))
prepend_path("PYTHONPATH", pathJoin(path, "/python-3.7/lib/python3.7/site-packages"))
setenv("PSI4ROOT", path)
