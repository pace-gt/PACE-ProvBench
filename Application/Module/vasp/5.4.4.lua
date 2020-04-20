local base = os.getenv("BENCH_INSTALL_ROOT")
local software = "vasp"
local version = "5.4.4"
local path = pathJoin(base, software, version)

help([[
  This module sets up vasp 5.4.4
  only licensed users can use this software, vasp is a modeling software used for molecular analysis, mainly used by researchers
]])

load("intel/19.0.3/mvapich2/2.3.1","Core/intel-parallel-studio/cluster.2019.3","fftw/3.3.8")
local compiler = "intel-19.0.3"
local mpi = "mvapich2-2.3.1"
path = pathJoin(path, mpi, compiler)

whatis("Sets up environment for "..software.." version "..version)

prepend_path("PATH", pathJoin(path, "/bin"))
setenv("VASPROOT", path)
