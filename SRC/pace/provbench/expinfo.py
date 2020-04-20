# -*- coding: utf-8 -*-
"""
Fang (Cherry) Liu (fang.liu@gatech.edu) 2019/10

pace.provbench.expinfo.ExpInfo is a main entry point for user to interact 
with PACE-ProvBench. 
The instance of this class will read in the application receipts base on
given application name. 
 
User code will instanticate this class for test runs, user at least needs
to specify the application name, then the default runtime and default 
application receipt will be used.
User can use the APIs to modify run time settings as nodes, ppns, walltime,
queues, run mode, etc 
"""


import pace.provbench.appinfo as appinfo
import pace.provbench.sysinfo as sysinfo
import pace.provbench.runtimeinfo as runtimeinfo
import pace.provbench.resultinfo as resultinfo
import pace.provbench.utilities as utilities
import pace.provbench.database as database
import time
import os
import subprocess

class ExpInfo:

    def __init__(self, app_name, input_runtime):
    
        """
        This method collects all the static information from user inputs and predefined
        receipt files, and inserts the data into the Experiments table. 
        :param app_name - string 
        The experiment instance will read the application receipt from default location
        ${RepoRootDir}/Application/Receipt/${app_name}.inp
        :param input_runtime - string file name with runtime input settings
         """
                
        self.database = database.DataBase()
        #self.database.connect()
        self.appInfo = appinfo.AppInfo()
        self.sysInfo = sysinfo.SysInfo()
        self.runtimeInfo = runtimeinfo.RuntimeInfo()
        self.resultInfo = resultinfo.ResultInfo()
        receipt = utilities.getRepoRootDir()+"/Application/Recipe/"+app_name.strip()+".inp"
        self.runtimeInfo.readConf(input_runtime)

        self.appInfo.readConf(receipt)
        """
        prepare the insertExperiment string
        (mode,software_name,software_version,scheduler,username,queue,nruns,nodes,ppns,ngpus)
        """
        appinfo_str = "(\"" + self.runtimeInfo.getMode() + "\",\""\
                          + self.appInfo.getAppName() + "\",\""\
                          + self.appInfo.getAppVersion() + "\",\""\
                          + self.runtimeInfo.getScheduler() + "\",\""\
                          + self.runtimeInfo.getUser() + "\",\""\
                          + self.runtimeInfo.getQueue() + "\",\""\
                          + str(self.runtimeInfo.getRuns()) + "\",\""\
                          + str(self.runtimeInfo.getTotalProcs()) + "\",\""\
                          + str(self.runtimeInfo.getGPUS())+"\")"
        self.database.insertExperiment(appinfo_str)

        # save the runtime information as global information
        self.resultInfo.saveRuntime(self.runtimeInfo)
        self.testDir = None  # directory for each experiment run
        self.rootDir = None # directory for all tests

    def constructExp(self, run):
        """ 
        args:
            run - record the nth run, mainly used for dumpsysinfo
        """
        runtime = self.runtimeInfo
        
        
        self.testDir = runtime.createAppTestdir(self.rootDir,self.appInfo.getAppName())

        runtime.constructMachineList()


        #if self.appInfo.getAppName() == 'namd':
        #    runtime.constructNamdNodefile(None)
        #else:
        runtime.constructNodefile()

        runtime.constructScript(self.appInfo)
        print("In EXP before constructInput")
        runtime.constructInput(self.appInfo)
       
        mode = runtime.getMode()


    def runExp(self):
        # actual runs
        runs = self.runtimeInfo.getRuns()
        mode = self.runtimeInfo.getMode()

        self.rootDir = self.runtimeInfo.createTopTestdir()
        runs = 1;
        for i in range(0, runs):
            self.constructExp(i)
            if i == 0:
                """
                only store app info for the first run
                """
                self.resultInfo.saveApp(self.appInfo)
            if mode == 'interactive':
                print('run in node mode')
                appName = self.appInfo.getAppName()
                command = 'cd '+self.testDir+';bash run'+appName+'.sh'
                print('run command '+command)
                subprocess.call(command, shell=True)



            else:
                print('run in queue mode')
                appName = self.appInfo.getAppName()
                #command = 'ssh '+self.runtimeInfo.getScheduler()+ ' \"cd '+self.testDir+';qsub run'+appName+'.pbs\"'
                command = 'cd '+self.testDir+';qsub -v SERVERHOST='+self.runtimeInfo.getScheduler()+' run'+appName+'.pbs'
                print('run command '+command)
                p = subprocess.run(command, shell=True, stdout=subprocess.PIPE)
                result = p.stdout.decode()
                #connectName=self.runtimeInfo.getUser()+'@'+self.runtimeInfo.getScheduler()
                #print('ssh to '+connectName)
                #sshProcess = subprocess.Popen(['ssh','%s' %connectName,
                #                      command],
                #                      stdin = subprocess.PIPE,
                #                      stdout = subprocess.PIPE,
                #                      universal_newlines = True, 
                #                      bufsize = 0
                #                      )
                #result = sshProcess.stdout.readlines()
                if result == []:
                #    error = sshProcess.stderr.readlines()
                    print("ERROR: %s" % error)
                else:
                    print("qsub -------------------%s\n" % result)
                    self.runtimeInfo.setJobIds(appName,result)
                    # parse log.out file to get module, md5 and hostinformation information
                    
                #store the jobid in hosts field for later query
                #runtimeInfo.setHosts(result)
                #subprocess.call(command, shell=True)
                time.sleep(60)
            
            if i == 0: 
                """
                log.out contains module list and md5 code
                """
                output = utilities.parseLogInfo(self.testDir+"/log.out")
                print(output)
                if 'modules' in output.keys():
                    self.appInfo.setModules(output['modules'])
                    self.database.insertModule(self.appInfo.getModules())
                if 'md5' in output.keys():
                    self.appInfo.setHashcode(output['md5'][0])
                    self.database.updateExperiment("md5", self.appInfo.getHashcode())

                """
                add application run script to DB
                """
                if mode == 'interactive':
                    scriptfile = open(self.testDir+"/run"+appName+".sh")
                else:
                    scriptfile = open(self.testDir+"/run"+appName+".pbs")

                script = "".join(scriptfile.readlines())
                #replace the double quote to single quote
                script = script.replace('"',"'")
                self.database.updateExperiment("script", script)

                """
                only output host information during the first run
                """
                if mode == 'queue':
                    """
                    At this point, the first run should already been finished, 
                    machine.list should exist as the format:
                    host1:ppn1
                    host2:ppn2
                    ...
                    """
                    print("-------------set hostfile")
                    self.runtimeInfo.setHostfile(self.runtimeInfo.getAppTestDir()+'/machine.list') 
              
                user = self.runtimeInfo.getUser()
                hosts = self.runtimeInfo.getHosts()
           
                for hostname in hosts:
                    self.sysInfo.dumpSystemInfo(user,hostname,\
                                                self.runtimeInfo.getAppTestDir(), self.runtimeInfo.getBenchRoot(),\
                                                self.database)




    def dumpResult(self):
        """        
        output the result, average number of runs in time (seconds)
        check failed or success
        Given the test directory, the summarized result is put under self.rootDir named summary.result
        each row contains <application name> <version> <# nodes> <# ppn> <time> <hostlist> <success/fail>
        """
        self.resultInfo.parseResult(self.rootDir,self.database)
    

    def setNNodes(self, nodes):
        self.runtimeInfo.setNNodes(int(nodes))

    def setPPNs(self, ppn):
        self.runtimeInfo.setPPNs(int(ppn))

    def stop(self):
        self.database.close();
