# -*- coding: utf-8 -*-
"""
Fang (Cherry) Liu (fang.liu@gatech.edu) 2019/10

AppInfo: 
This class containts the data for each individual application,
the instance of this class is one application. 
"""


from collections import defaultdict
import subprocess
import pace.provbench.utilities as utilities
import pace.provbench.command as command

class AppInfo:

    # instance attributes
    def __init__(self):
        """
        Local data members:
            myvars: stores all input data reading from application recipe file
            hashcode: the md5 hash code for application's binary executable
            name: the application name (e.g. leslie-spec)
            version: the application software version (e.g. 21Feb19)
            inputDir: the given application's input files location, default is <bench root>/Application/Input
            exe:    the application's executable name (e.g. les3d.x)
            modulePath: user specified the MODULEPATH, need to append to the existing MODULEPATH, by default, 
                        it is set by run `source <bench root>/Utilities/bench_init.sh`
            benchRoot: is the root directory of PACE_ProvBench's local copy from git clone
            command: the command line to run the application (e.g. time mpirun -np $NPROCESS -f $MACHINEFILE $EXE)
            modules: the compiler used to build application executable (e.g. intel/19.0.3)
                     the mpi libaries used to build application executable (e.g. mvapich2/2.3.1)
                     dependend libraries (e.g. mkl/2019.03,fftw/3.3.8,hdf5/1.10.0), this is a comma separated list
        """
        self.myvars = None
        self.hashcode = None
        self.name = None
        self.version = None
        self.inputDir = None
        self.exe = None
        self.modulePath = None
        self.benchRoot = utilities.getBenchRootDir()
        self.command = command.Command()
        self.modules = None
    
    def readConf(self, input):
        """
        This method reads in application configuration from a given recipe, the default location of recipe 
        is under <bench root>/Application/Recipe, and use the configuration to setup a appInfo object, 
        
        :param input: application recipe, the naming convension is <app name>.inp, the file format is 
        key<delimiter>value, and ignors the line starting with # and [
        
        :return this method fillup self.myvars 
        """
      
        # print('read conf file from ', input)
        self.myvars = utilities.parseFile(input,'=')
        if 'appname' in self.myvars.keys():
            self.name = self.myvars['appname'][0].strip()
        if 'version' in self.myvars.keys():
            self.version = self.myvars['version'][0].strip()
        if 'inputdir' in self.myvars.keys():
            self.inputDir = self.myvars['inputdir'][0].strip()
            print(self.inputDir)
        else:
            self.inputDir = self.getInputDir()

        if 'modulepath' in self.myvars.keys():
            self.modulePath = self.myvars['modulepath'][0].strip()
        else:
        	self.modulePath = self.benchRoot+'/Application/Module'

        # construct the command from recipe
        self.command.constructCommand(input)


    def outLog(self,exe):
        """
        This method constructs partial script for runtime script, and it is used to capture the md5 hashcode
        for the application
        
        :param exe: executable name (e.g. les3d.x)
        
        :return rstr: a formated string ready to be put in the run script
        """
        rstr = 'EXE=`which '+exe+'`>> log.out\n'
        rstr += 'echo $EXE>>log.out\n'
        rstr += 'ldd $EXE >>log.out\n'
        rstr += 'echo md5 >>../log.out\n' #md5 keyword matchs utilities.parseLogout() and db schema
        rstr += 'echo `which '+exe+'|xargs md5sum|cut -d \" \" -f1` >>../log.out\n'
        return rstr

    def resultLog(self):
        """
        This method constructs partial script for runtime script, and it captures the return code of the 
        experiment run. 
        
        :return a formated string ready to be put in the run script
        """
        return 'echo \"Running success(0)/failed(1) \"${PIPESTATUS[0]} >>log.out'	

    def constructCommand(self):
        """
        This method constructs the all related information for the command based on a given executable
        
        :return a formated string ready to be put in the run script
        """
        exe = self.command.getExe()
        rstr = self.outLog(exe)
        #rstr += 'echo '+self.command.getCommand()+'>>log.out\n'
        rstr += self.command.getCommand()+'\n'
        rstr += self.resultLog()		
        return rstr

    def constructModule(self):
        """
        This method builds the module load command based on the software name and version given in the recipe file,
        and the assumption is that the corresponding module file 
        <bench root>/Application/Module/<software name>/<software version>.lua
        exists. 
        This method also update the user environment by sourcing bench_init.sh
        
        :return a format string ready to be put in the run script
        """
        print(self.myvars['module'])
        rstr = 'source '+self.benchRoot+'/Utilities/bench_init.sh\n'
        #rstr +='module purge\n'
        for m in self.myvars['module']:
            rstr += 'module load '+m+'\n'
        rstr += 'module load '+self.name+'/'+self.version+'\n'
        rstr += 'modulecount=`module -t list 2>&1|wc -l`\n'
        rstr += 'echo modules:$modulecount >& log.out\n' #modules keyword matches utilities.parseLogout()
        rstr += 'module -t list 2>>log.out'
        return rstr


    def generateHashcode(self, app):
        """
        This method calculates the md5 hash code for the application
        
        :param app: executable with the full path
        
        :return a string with md5 code
        """
        result = subprocess.run(['md5', app], stdout=subprocess.PIPE)
        return result.stdout

    def setModules(self, modules):
        """
        This method sets up modules list

        :param modules: a List of strings
        """
        self.modules=modules

    def getModules(self):
        return self.modules

    def setHashcode(self,hashcode):
        """
        This method sets up hashcode
        :param hascode: a string
        """
        self.hashcode=hashcode

    def getHashcode(self):
        """
        This method returns hashcode
        :return string
        """
        # hashcode is a string
        return self.hashcode

    def getAppName(self):
        """
        This method return application name
        :return string
        """
        return self.name.strip()

    def getAppVersion(self):
        """
        This method return application version 
        :return string
        """
        return self.version

    def getExe(self):
        """
        This method return application executable name 
        :return string
        """
        return self.exe

    def getModulePath(self):
        """
        This method return module path 
        :return string
        """
        return self.modulePath

    def getCommand(self):
        """
        This method return command to run the experiment 
        :return string
        """
        return self.command.getCommand()

    def getRepoRoot(self):
        """
        This method return the full path to benchmark root directory 
        :return string
        """
        return self.benchRoot

    def getInputDir(self):
        """
        This method return the full path to application input's directory 
        :return string
        """
        if self.inputDir == None:
            self.inputDir = self.benchRoot+'/Application/Input/'+self.getAppName()
        return self.inputDir

