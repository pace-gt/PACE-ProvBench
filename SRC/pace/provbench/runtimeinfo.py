# -*- coding: utf-8 -*-
"""
Fang (Cherry) Liu (fang.liu@gatech.edu) 2019/11

RuntimeInfo:
This class containts the runtime information for each individual experiment.
"""

from collections import defaultdict
import pace.provbench.utilities as utilities

class RuntimeInfo:

    def __init__(self):

        """
    	This method initializes class data members, and set up the default values if it's necessary
	    modeList: List, includes two run modes, default value are  interactive and queue
	    user: String, who runs the experiments, the value is set through input.runtime, and it is used to 
		    create the experiment test directiory and as a user to run a test in the queue mode
	    queue: String, which queue is used when running in the queue mode, it is used to create the experiment
		    test directory and #PBS -q <queue> in the queue modea
	    nodes: Integer, how many nodes the experiments runs, in the interactive mode, it first reads nodes
		    setting in input.runtime, if it doesn't match number of lines in input.hosts file, it will 
		    throw an error. In the queue mode, nodes setting in the input.runtime is used to created 
		    PBS file in #PBS -l nodes=<nodes>, but the actaul nodes field in the DB Experiments table
		    will use the actual # of nodes during the runtime
	    ppns: List, it records the ppns per node
	    gpus: Integer, # of gpus is used, mainly for queue module testing 	
	    runs: Integer, how many repeated runs per each experiment
	    rootTestDir: String, default is this <git repo root>/Test, user can overwrite it through input.runtime
			roottestdir value
	    host: List, it stores all hosts used in one experiment
	    benchRoot: String, git repo root directory
	    jobids: Dictionary, e.g. {'app1':[1,2,3],'app2':[4,5,6]}, store the job IDs for each experiment
 	    appTestDir: String, <rootTestDir>/<timestamp>/<AppName>, there are number of subdirectories inside this 
		    directory to have mutliple runs		
	    nodeFile: String, file is generated from either input.hosts or PBSNODEFILE
	    scriptFile: File generated to run the experiment
	    nprocs: Integer, total number of processes used
	    hostfile: String, either input.hosts or generated from PBSNODEFILE	
	    """ 
        
        self.myvars = defaultdict(list)
        self.modeList = ['interactive','queue']

        # user specified parameters, can be modified by receipt and setter methods

        self.mode = "interactive"
        self.user = "root"
        self.queue = None
        self.nodes = 1
        self.ppns = [] 
        self.gpus = 0
        self.runs = 1
        self.rootTestDir = utilities.getRepoRootDir()+"/Test"    # default <repo root>/Test
        self.hosts = None # this field will be a list
        self.benchRoot = utilities.getRepoRootDir()
        
        # Data generated from experiments
        self.jobids = defaultdict(list) 
        self.parentTestDir = None     # root directory for a given test with timestamps
        self.appTestDir = None     # the directory where test actually runs with application names

        self.timeStamp = None
        self.testDate = None
        self.testTime = None # will be set when create testRootDir
        self.nodeFile = None
        self.scriptFile = None
        self.nprocs = 0 
        self.hostfile = None


    def readConf(self, input_runtime):
        """
	    This method reads in input_runtime, and setup what user specified entry in the corresponding 
	    fields
	    self.myvars : dictionary with a list as the value, it stored all input values
			and all fields are cast into correct data type to class data members
        """

        print("read conf file from ", input_runtime) 

        self.myvars = utilities.parseFile(input_runtime,'=')

        if 'mode' in self.myvars.keys():
            self.mode = self.myvars['mode'][0].strip()
        if 'user' in self.myvars.keys():
            self.user = self.myvars['user'][0].strip()
        if 'queue' in self.myvars.keys():
            self.queue = self.myvars['queue'][0].strip()
        if 'nodes' in self.myvars.keys():
            self.nodes = int(self.myvars['nodes'][0].strip())
        if 'ppn' in self.myvars.keys():
            for x in range(self.nodes):
                self.ppns.append(self.myvars['ppn'][0])
            print("read from ppns conf ",self.ppns)
        if 'runs' in self.myvars.keys():
            self.runs = self.myvars['runs'][0].strip()
        if 'hostfile' in self.myvars.keys():
            self.hostfile = self.myvars['hostfile'][0].strip()
        if 'scheduler' in self.myvars.keys():
            if self.queue == None:
                print('Please give a queue if you are using a scheduler')
                exit(2)
            else:
                self.scheduler = self.myvars['scheduler'][0].strip()
        if 'roottestdir' in self.myvars.keys() and self.myvars['roottestdir'][0].strip() != "":
            self.rootTestDir = self.myvars['roottestdir'][0].strip()
        else:
            self.rootTestDir = utilities.getBenchRootDir()+'/Test'
            self.myvars['roottestdir'].append(self.rootTestDir.strip())

        if self.mode == 'interactive': 
            # The code will stop if hostfile doesn't exist 
            file = utilities.isFileExist(self.hostfile)

            nhosts = len(open(self.hostfile).readlines())
            #print(str(nhosts)+' '+str(self.nodes))
	    # The code will stop if # of lines in hostfile doesn't match nodes specified in input.runtime 
            if nhosts != int(self.myvars['nodes'][0]):
                print('In interactive mode, # of hosts should be echo to nodes from input.runtime')
                #self.myvars['nodes'][0]=nhosts
               	exit(1) 
            """ 
            read input.hosts file, assume input.runtime already setup ppn across all nodes,
            now needs to update the ppn based on what is specified in input.hosts
            """
            i = 0
            for line in file:
                host,ppn = line.strip().partition(':')[::2]
                self.myvars['hosts'].append(host)
                print(len(self.ppns))
                if ppn != None:
                    self.ppns[i] = ppn
                    i += 1 
            file.close()
	    # this data member store list of hosts
            self.hosts = self.myvars['hosts']
            print("After reading from input.hosts")
            print(self.ppns)


    def constructMachineList(self):
        """
	    This method constructs the machine file from input.hosts for interactive mode,
	    file format:
	    host1:ppn1
	    host2:ppn2
        """
        mode = self.mode
        if mode == self.modeList[0]: #interactive mode
            print(self.myvars['hosts'])
            outfile = open(self.appTestDir+'/machine.list', 'w')
            index=0
            for m in self.myvars['hosts']:
                outfile.write(m + ':' + str(self.ppns[index]) +'\n')
                index += 1
            outfile.close()

    def constructNodefile(self):
        """
	    This method builds nodefile similar to PBS_NODEFILE, and put it under testdirectory
	    for interactive mode
        # mode : string
        # hosts : list
        # ppns : string
        # mode can be queue or node
        """
        mode = self.mode

        if mode == self.modeList[1]: #queue mode
            self.nodeFile='$PBS_NODEFILE'
            #self.nprocs = utilities.countFileLen(self.nodeFile)
        elif mode == self.modeList[0]: #interactive mode
            self.nodeFile = self.appTestDir+'/nodefile'
            outfile = open(self.nodeFile, 'w')
            index = 0
            for m in self.hosts:
                pn = int(self.ppns[index])
                index += 1
                for n in range(pn):
                  outfile.write(m + '\n')
            outfile.close()
            self.nprocs = utilities.countFileLen(self.nodeFile)
        else:
            print('please specify queue or interactive in mode')
            exit(2)

        return self.nodeFile

    def constructNamdNodefile(self, pbsnodefile):
        """
	TODO:
	NAMD has specified nodefile format
	"""
        pn = int(self.ppns)
        mode = self.mode
        self.nodeFile = self.appTestDir+'/nodefile'
        outfile = open(self.nodeFile, 'w')
        outfile.write("group main\n")
        if mode == self.modeList[1]: #queue mode
	    #this the queeue mode $PBSNODEFILE won't exist when contructing the 
            #script -  Cherry 05/01/2019
            #for m in pbsnodefile:
            #    outfile.write("host " + m + '\n')
            # todo construct the nodefile in str and add into the script
            print('I am in queue mode for NAMD')
        elif mode == self.modeList[0]: #interactive mode
            for m in self.hosts:
                for n in range(pn):
                    outfile.write("host "+m+'\n')
            outfile = open(self.nodeFile, 'w')
            self.nprocs = utilities.countFileLen(self.nodeFile)-1
        else:
            print('please specify queue or interactive in mode')
            exit(2)


    def createAppTestdir(self, parentdir, appname):
        """
	    This method creates testdir for each application,
	    there will be multiple subdirectoies under this directory for repeated runs
        """
        #timeStamp,mdate,mtime = utilities.getTimeStamp()
        #self.testDate = mdate
        #self.testTime = mtime
        #dirPath = parentdir +'/'+appname+'/'+timeStamp
        dirPath = parentdir +'/'+appname
        self.appTestDir = utilities.createDir(dirPath)
        return self.appTestDir


    #create test dir under user specified path, assume all inputs already exists
    def createTopTestdir(self):

        """
	    This method creates a top test directory in which each subdirectory have one application run,
	    the result is passed to createAppTestdir method as parentdir argument
        """
        timeStamp,mdate,mtime = utilities.getTimeStamp()
        self.testDate = mdate
        self.testTime = mtime
        self.timeStamp = timeStamp
        print(self.rootTestDir)
        print(self.user)
        print(self.nodes)
        print(self.ppns)
        print(timeStamp)
        if self.mode == self.modeList[0]: # interactive mode
            dirPath = self.rootTestDir+'/'+self.user+'-'+self.getFirstHost()+'-'+str(self.nodes)+'-'+str(self.ppns[0])+'-'+timeStamp
        else:
            dirPath = self.rootTestDir+'/'+self.user+'-'+self.getQueue()+'-'+str(self.nodes)+'-'+str(self.ppns[0])+'-'+timeStamp
        self.parentTestDir = utilities.createDir(dirPath)
        return self.parentTestDir

    def constructInput(self,appinfo):
        """ 
 	    This method copied the input file from default input directory. <Bench Root>/Application/Input	
	    :params appinfo, instance of AppInfo class
        """
        inputDir = appinfo.getInputDir()
        utilities.copyDir(inputDir,self.appTestDir)

    def constructScript(self, appinfo):
        # queue mode : .pbs script
        # node mode : .sh script
        # appinfo : object contains all application related information

        mode = self.mode
        appname =  appinfo.getAppName()
        if mode == self.modeList[1]:#queue mode

            print('construct a queue script')
            self.scriptFile = self.appTestDir + '/run'+appname+'.pbs'
            
            scriptfile = open(self.scriptFile, 'w')
            scriptfile.write('#This is a generated script\n')
            if appname == 'cudabench':
                scriptfile.write('#PBS -l nodes='+str(self.nodes)+':ppn=1:gpus=1\n')
            else:
                scriptfile.write('#PBS -l nodes='+str(self.nodes)+':ppn='+str(self.ppns[0])+'\n')
            scriptfile.write('#PBS -l walltime=1:00:00 \n')
            scriptfile.write('#PBS -q '+self.queue+'\n')
            scriptfile.write('#PBS -j oe \n')
            scriptfile.write('#PBS -o '+appinfo.getAppName()+'.output.$PBS_JOBID\n')
            scriptfile.write('cd $PBS_O_WORKDIR \n')
            scriptfile.write(appinfo.constructModule()+'\n')
            scriptfile.write('MACHINEFILE='+self.nodeFile+'\n')
            scriptfile.write('NPROCS=`wc -l < ${PBS_NODEFILE}`\n')
            scriptfile.write('`cat ${PBS_NODEFILE}>nodefile`\n')
            scriptfile.write('`'+self.benchRoot+'/Utilities/construct_machine.sh`\n')
            
            scriptfile.write('for i in {1..'+self.runs+'};do\n')
            scriptfile.write('tempdir=run$i+`date +%Y-%m-%d_%H_%M`\n')
            scriptfile.write('mkdir $tempdir \n')
            scriptfile.write('cd $tempdir \n')
            scriptfile.write('cp -r ../* .\n') 
            scriptfile.write(appinfo.constructCommand()+'\n')

            scriptfile.write('cd ../ ;done\n')

            scriptfile.close()

        elif mode == self.modeList[0]: # interactive mode

            self.scriptFile = self.appTestDir + '/run'+appinfo.getAppName()+'.sh'
            scriptfile = open(self.scriptFile, 'w')
            scriptfile.write('#This is a generated script\n')
            #scriptfile.write('export MODULEPATH='+appinfo.getModulePath()+':$MODULEPATH\n')
            scriptfile.write(appinfo.constructModule()+'\n')
            scriptfile.write('MACHINEFILE='+self.nodeFile+'\n')
            scriptfile.write('NPROCS='+str(self.nprocs)+'\n')
            scriptfile.write('for i in {1..'+self.runs+'};do\n')
            scriptfile.write('tempdir=run$i+`date +%Y-%m-%d_%H_%M`\n')
            scriptfile.write('mkdir $tempdir \n')
            scriptfile.write('cd $tempdir \n')
            scriptfile.write('cp -r ../* .\n')
            scriptfile.write(appinfo.constructCommand()+'\n')

            scriptfile.write('cd ../ ;done\n')

            scriptfile.close()
        else:
            print('Please specify mode in either queue or interactive')
            exit(2)
    
    
    def getTotalProcs(self):
        return self.nprocs

    def getTimeStamp(self):
        return self.timeStamp

    def getUser(self):
        return self.user

    def getFirstHost(self):
        return self.hosts[0]

    def getHosts(self):
        return self.hosts

    def setHostfile(self,hostfile):
                         # + str(self.runtimeInfo.getPPNs()[0]) + "\",\""\
        """
        only called by queue mode
        This function assume hosts is a file with format
        host1:ppn
        host2:ppn
        the file is parsered and update self.hosts
        """
        self.hostfile = hostfile
        file = open(hostfile)
        """
        refresh hostlist
        """
        self.hosts=[]
        i = 0
        nproc = 0
        self.ppns=[]
        for line in file:
            host,ppn = line.strip().partition(':')[::2]   
            self.hosts.append(host)             
            
            if ppn != None:                    
                print("ppn------------------------"+ppn)
                self.ppns.append(ppn)            
                print(self.ppns)
                nproc += int(ppn)
                i += 1                     
        self.nprocs=nproc

        self.nodes=i
        file.close()

    def getJobIds(self,appname):
        return ','.join(self.jobids[appname.strip()]).split('.')[0]

    def setJobIds(self,appname,jobid):
        #this method stored jobids for each run per application
        #in a dictionary
        self.jobids[appname.strip()].append(jobid.strip())

    def getRootTestDir(self):
        return self.rootTestDir

    def getBenchRoot(self):
        return self.benchRoot


    def getGPUS(self):
        return self.gpus

    def getNNodes(self):
        return self.nodes

    def setNNodes(self, nodes):
        self.nodes = nodes

    def getPPNs(self):
        # for queue mode, setHost
        #ppns=""
        #for i in range(int(self.nodes)):
        #    ppns += self.ppns[i]+','
        return self.ppns

    def setPPNs(self, ppn):
        print("overwrite ppn as "+str(ppn))
        if len(self.ppns) < self.nodes:
            exit("Command line specified number of nodes must be smalled than what's in input.runtime")
        for m in range(self.nodes):
            self.ppns[m]=str(ppn)
    def getAppTestDir(self):
        return self.appTestDir

    def getMode(self):
        return self.mode

    def getRuns(self):
        return int(self.runs)

    def getScheduler(self):
        return self.scheduler
    def getQueue(self):
        return self.queue

    def getDate(self):
        return self.testDate

    def getTime(self):
        return self.testTime
