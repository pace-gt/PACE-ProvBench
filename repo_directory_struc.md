
# Top level directory structure:

```
PACE-ProvBench
    | - Application (see details below)
    | - SRC (python source code, see details below)
    | - Utilities (see details below)
    | - UnitTest (see details below)
    | - Design (design documenation, includes project report which covers the overall structure in PPT)
    | - Images (pictures for the documentation)
    | - Test (place holder for default test running location)

```



### Application : include every information relates to applications

```
Application
  | - Source (stores all pace_build.sh scripts for test suite applications, e.g. Source/leslie-spec/pace_build.sh)
  | - Module (module files for test suite applications, e.g. Module/leslie-spec/21Feb19.lua)
  | - Install (stores all binaries, not necessary in git, e.g. Install/leslie-spec/21Feb19/)
  | - Input (input files for each application, e.g. Input/leslie-spec/)
  | - Recipe (all application related information, modules and command line, e.g. Recipe/leslie-spec.inp)
```

### SRC : includes framework source code in a python package 

```
SRC
  | - pace
    | - provbench
      | - appinfo.py (encapsulates application related information, e.g. software_name, version, etc.)
      | - expinfo.py (the object contains all other objects, and called by ../UnitTest/runtest.py)
      | - __init__.py 
      | - resultinfo.py (stores and processes result collecting, e.g. average_time, etc.)
      | - runtimeinfo.py (encapsulates runtime configuration for each test, e.g. nodes, ppn, queue, scheduler)
      | - sysinfo.py (collects the system informaiton and insert into database)
      | - utilities.py (auxiliary functionalities)
      | - database.py (interacts with database, query, insert and update)
      | - command.py (encapsulates the command line from Recipe)

```

### UnitTest : include all test scripts and inputs, user will run the tests and queries from this directory

```
UnitTest
  | - input.hosts (stores machine list in a format each line contains one host)
      e.g. rich133-s40-17-skylake:12
           rich133-s40-16-skylake:24
  | - input.runtime (configuration on resources)         
  | - runtest.py (master script to invoke the tests)
  | - runquery.py (query the database)
  | - notes (basic steps to use the above scripts)
```

### Utilities: Auxiliary functions, e.g. boot strap, data base creation

```   
Utilities
  | -  bench_init.sh (boot strap script to setup the framework enironments)
  | -  create_db.sh (create database for fresh install, only need to be done once)
  | -  create_provbench_table.mysql (create the tables, used in create_db.sh)
  | -  construct_machine.sh (called by SRC/pace/provbench/runtimeinfo.py to construct the machine.list)
  | -  get_sysinfo.sh (called by SRC/pace/provbench/sysinfo.py to collect the system information)
  | -  spack-config (includes all spack related configuration to create the software stack for testing suite, check README.md to find the details)
  ```
### SC20Test: all automation test scripts and test results
```
SC20Test
    | - performance(performance test scripts and results)
    | - overhead (overhead test scripts and results)
```
