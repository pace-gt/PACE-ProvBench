local base = os.getenv("BENCH_INSTALL_ROOT") 
local name = "anaconda3"
local version = "2019.07"
local root = pathJoin(base,  name, version)

setenv("ANACONDA3ROOT", root)
setenv("PYTHONROOT", root)

prepend_path("PATH", pathJoin(root, "condabin"))
prepend_path("PATH", pathJoin(root, "bin"))
