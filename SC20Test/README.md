# Test for SuperComputing 2020 Paper
In this directory, we include all test automation scripts as well as some results for reproducibility. 

## Directory Structure:
There are two folders:
- performance folder is used to run performance test on two Intel systems, the results are shown in paper's Test section. 
```
performcne - this folder includes test scripts for both Casecade Lake and Broadwell systems. 
  | - codatest.sh is used for testing Casecade Lake system in queue mode 
  | - input.runtime.coda is the runtime configuration for script codatest.sh
  | - codatest-interactive.sh is used for testing Casecade Lake system in interactive mode while no job scheduler is used.
  | - input.runtime.interactive is the runtime configuration for script codatest-interactive.sh
  | - input.hosts is the runtime host configuration for script codatest-interactive.sh
  | - rh7test.sh is used for testing Broadwell systems in queue mode  
  | - input.runtime.rh7 is the runtime configuration for script rh7test.sh
  | - <app>.<node>.<ppn>.coda files are the output in queue mode
  | - <app>.<node>.<ppn>.interactive files are the output in interactive mode
```

- overhead folder is used to run the test without using PACE-ProvBench, there are two modes for this test, 
one in the queue mode, another is interactive mode. As paper only includes the overhead on Casecade Lake system. 
The automation script runs on Casecade Lake only. The resuls are shown in paper's Test section. 
```
overhead 
  | - run.sh is used for testing in queue mode, it invokes all <app>.pbs scripts
  | - overhead.test is generated by run.sh, it recalls the jobID information, and it is used to determine the actual runtime.
  | - run-interactive.sh is used to test in interactive mode without queue, it invokes all <app>.sh scripts
  | - hosts.<nodes> are files for interactive run scripts and serves as the machinefile for mpirun
  | -  <app>.<node>.<ppn>.interactive files are the output in interactive mode
```
## Run the test
Before running the test, be sure all applications are correctly installed and can be loaded by modulel load. 
All application input are included in <PACE-ProvBench>/Application/Input. You may need to update the application
recipe under <PACE-ProvBench>/Application/Recipe, and change the test scripts accordingly if you want to use 
the different name for the recipes. 
  
- To run the performance test in queue mode for Casecade Lake system:
```
cd performance
../../Utilities/bench_init_withoutspack.sh
./codatest.sh
```
- To run the peformation test in interactive mode for Casecade Lake system, update input.hosts with the host you
want to run test on:
```
cd performance
../../Utilities/bench_init_withoutspack.sh
./codatest-interactive.sh
```

- To run the performance test in queue mode for Broadwell system:
```
cd performance
../../Utilities/bench_init_withoutspack.sh
./rh7test.sh
```

- To run the overhead test for non-provbench in queue mode
```
cd overhead
../../Utilities/bench_init_withoutspack.sh
./run.sh
```

- To run the overhead test for non-provbench in interactive mod
```
cd overhead
../../Utilities/bench_init_withoutspack.sh
for i in 1 2 4; do ./run-interactive.sh $i `pwd`;done
```