## Run tests as User

This session describes how User can run test, there are two running modes: interactive and queue mode. 
For interactive mode, user needs to specify the host file namely `input.hosts`. 
For queue mode, user needs to specify the queue name, scheduler name, user name and mode. 

### Input files to run the test
Example input files are under $PACEPROVBENCH/UnitTest

* cd UnitTest
* input.hosts # this is the file contains the machine list, each line has one machine, 
no comments can be added to this file, e.g.
`host1:24`
* input.runtime # this file determines the runtime parameters for experiments for a given application

Keywords in input.runtime (please note # means comments)
```
[Mode] # either interactive or queue
    mode=queue # required for both modes

[Queue]
    user=<userid> # required for both modes
    queue=<queue name>  # required for queue mode
    scheduler=<scheduler name> # required for queue mode

[Common]
    nodes=1  # required for both modes
    ppn=24   # required for both modes
    runs=2   # required for both modes, if nothing is specified, the default 1 will be used
    hostfile=./input.hosts  # required for interactive mode, user can use any file name here, and use absolute path 
    roottestdir=[absolute path or default is ../Test] # specify where to put the output files
```

### How to run the test
If user runs a test in a queue mode, one needs to login to the nodes who connect to a scheduler, for PACE in house
test, please use `testflight-login.pace.gatech.edu` to run the test. 

If user runs a test in an interactive mode, one can login to any RHEL7 nodes with a proper network filesystem mount 
(e.g. GPFS native client)

```
cd UnitTest
source ../Utilities/bench_init.sh
module load anaconda3/2019.07
python3 runtest.py --app "leslie-spec" --runtime "./input.runtime" --nodes 2 --ppn 12
```
Arguments list:
* app : double quote around string with application name `<app name>`, and it should match on all places, modules, recipes
* runtime : input file to specify the runtime configuration for the test, e.g. `<abosulte path>/input.runtime`
* nodes : user can specify how many nodes to use, this setting overwrite what's put in `input.runtime` file. 
* ppn : processes per node, this setting overwrite what's put in `input.runtime` file. 

## Test Directory
Whenever there is a test running, user can specify the `roottestdir` in `input.runtime`, or system default test location will be used `<repo root>/Test`, and test directory with encoded information will be created under test root directory, e.g.
in the format of
```
<user name>-<first host name>-<#nodes>-<#ppn>-<timestamp>
```
* under this directory, each application will have their own test directories:
```
<user name>-<first host name>-<#nodes>-<#ppn>-<timestamp>
  | - leslie-spec
    | - run1+2019-12-02_17_51
    | - run2+2019-12-02_17_52
  | - summary.result  
```
each run output data is stored in time.output file. 
The test result stores in summary.result under root test directory.
And the detailed result is also inserted into the database.

## Example output - summary.result

```
----------------------------------------------------------------------------------------------------------------------------------
Timestamp   |Application Name|     Version|      # runs|     # nodes|       # ppn|Average Run time (seconds)|Maximum Run time (seconds)|Minimum Run time (seconds)|Run time Stand-deviation|   Run time Variance|            Hostlist|Success (0)/Failed(1)|
----------------------------------------------------------------------------------------------------------------------------------
20191202_17_51_52| leslie-spec|     21Feb19|           2|           2|       12,12|              16.695|              16.814|              16.576|               0.119|0.014160999999999946|['host1', 'host2']|   0
```

For test in queue mode, the summary.result is:
```
----------------------------------------------------------------------------------------------------------------------------------
Timestamp   |Application Name|     Version|      # runs|     # nodes|       # ppn|Average Run time (seconds)|Maximum Run time (seconds)|Minimum Run time (seconds)|Run time Stand-deviation|   Run time Variance|            Hostlist|Success (0)/Failed(1)|
----------------------------------------------------------------------------------------------------------------------------------
20191204_08_51_30| leslie-spec|     21Feb19|           2|           1|          24|             21.2025|              21.325|               21.08|              0.1225|0.015006250000000122|['host3']|   0
```
The above test showed us that 2 12 core Cascade Lake nodes run faster than 1 Skylake node.
