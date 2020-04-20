local base = os.getenv("BENCH_INSTALL_ROOT")
local software = "espresso"
local version = "6.4"
local path = pathJoin(base, software, version)

help([[
  This module sets up Quantum ESPRESSO 6.4.
  Download: https://www.quantum-espresso.org/download
]])

load("Core/intel-parallel-studio/cluster.2019.3","intel/19.0.3/mvapich2/2.3.1")
local compiler = "intel-19.0.3"
local mpi = "mvapich2-2.3.1"
path = pathJoin(path, mpi, compiler)

whatis("Sets up environment for "..software.." version "..version)

prepend_path("PATH", pathJoin(path, "/bin"))

setenv("ESPRESSOROOT", path)
