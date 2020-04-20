local base = "/gpfs/pace1/project/pace-admins/Benchmarks/CascadeLake/Install"
local software = "osu-micro-benchmarks"
local version = "5.4.3" 
local path = pathJoin(base, software, version)

help([[
     This is osu-micro-benchmarks for Benchmarks.
]])


prereq_any("intel/19.0")
prereq("mvapich2/2.3")
local compiler = os.getenv("COMPILERVERSION")
local mpi = os.getenv("MPIVERSION")
path = pathJoin(path, mpi, compiler)

whatis("Sets up environment for "..software.." version "..version)


prepend_path("PATH",pathJoin(path,"libexec/osu-micro-benchmarks/mpi/collective")) 
prepend_path("PATH",pathJoin(path,"libexec/osu-micro-benchmarks/mpi/one-sided")) 
prepend_path("PATH",pathJoin(path,"libexec/osu-micro-benchmarks/mpi/pt2pt")) 
prepend_path("PATH",pathJoin(path,"libexec/osu-micro-benchmarks/mpi/startup")) 

setenv("OSUROOT", path)
