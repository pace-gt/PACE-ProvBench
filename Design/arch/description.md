### PACE-ProvBench Design details

1.	There will be three different roles interacting with PACE-ProvBench

* Developer:  Fang (Cherry) Liu

* Contributor: Any PACE software team members and people from other HPC centers

* User: who runs the PACE-ProvBench, and build test suites with SPACK, including PACE members and people from other HPC centers 

2.	The way three roles interact with PACE-ProvBench are:

* Developer is responsible to extend the framework with more object friendly way, which allows future extension, and document the code in the function level

* Contributor: Provide the reciepts for each individual applications and those receipts won't be modified by users. Receipts are to specify the runtime/application configuration file to run the tests, also be able to follow the instruction to rebuild the software stack with SPACK and manual build the software as needed 

* User: Able to specify runtime configuration e.g. nodes, ppns, queue, testdir, moduledir etc, through APIs to run the tests, and user can also build test suites using SPACK by following the instruction

3.	Both Contributors and Users only need to deal with one class ExpInfo, and this class takes into one single file per application called receipt, and the performance data will be injected into the database 
And all receipts will be kept under `<repo root>/Application/Receipts` with naming convertion as `<app name>.inp`, `<app name>` list will be predefined list, if the application name is not in the predefined list, the experiment won't run


4.	Make AppInfo class generic enough without all application specific information

6.	Create a new class `Command` to construct the command line commands, e.g. `<timer> <launcher> <launcher args> <exe> <exe args> <redirect> <outputfile>` 
  
* `<launcher>` can be mpirun, charmrun
  
* launcher args can be : 

 `–np <processes> -machinefile <filename>`
  
* exe args can be:

 `–nodisplay –SingleThreaded (for matlab)`

7. Create a new class `BenchDB` to act as query construction for database interaction

8. hostfile format, two type of formats will be accepted, 
```
hostname1
hostname2
```
The ppn will be used either from default setting or from receipts, or from API setters. 
```
hostname1:ppn1
hostname2:ppn2
```
this hostfile format will allow user to specify the different ppns per node, and API setters will be used to update ppns. (need more discussion though)

9. systemInfo insertion will happen before job runs, in this way, both interactive and queue modes will have two steps DB update, record insertion during the systmeInfo collection, and record update at the end of all runs. 

10. Each experiment will have multiple records, based on how many machines it used. 

11. Only SPACK configuration will be kept in the repository, user can specify the SPACK source, binary and module directory. The default locations will be inside `<repo root>/Application`
