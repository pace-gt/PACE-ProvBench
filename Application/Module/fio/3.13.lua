local base = os.getenv("BENCH_INSTALL_ROOT")
local software = "fio"
local version = "3.13"
local path = pathJoin(base, software, version)

help([[
This module sets up fio 3.13
To load in the module do:
        module load fio/3.13
]])

whatis("Sets up environment for "..software.." version "..version)

prepend_path("PATH", pathJoin(path, "/bin/"))
prepend_path("MANPATH", pathJoin(path, "/man/"))
setenv("FIOROOT", path)
