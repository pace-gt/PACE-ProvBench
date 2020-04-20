local base = os.getenv("BENCH_INSTALL_ROOT")
local software = "psi4"
local version = "1.3.2"
local path = pathJoin(base, software, version)

help([[
  This module sets up psi4 version 1.3.2 

	1) First do:
		module load psi4/1.3.2
    2) Setup the scratch loaction (the default is /scratch)
      export PSI_SCRATCH=<location>
	2) Next, use the command:
		psi4 input-file-of-any-name-or-extension -n<#cores>
]])


load("Core/intel-parallel-studio/cluster.2019.3","anaconda3/2019.07")
local compiler = "intel-19.0.3"
path = pathJoin(path, compiler)

whatis("Sets up environment for "..software.." version "..version)

prepend_path("PATH", pathJoin(path, "/bin"))
prepend_path("PYTHONPATH", pathJoin(path, "/python-3.7/lib/python3.8/site-packages"))
setenv("PSI_SCRATCH","/scratch")
setenv("PSI4ROOT", path)
