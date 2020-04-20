# -*- coding: utf-8 -*-
"""
Fang (Cherry) Liu (fang.liu@gatech.edu) 2019/10

Command: 
This class contains the command line for each application
e.g. time mpirun -np $NPROCESS -f $MACHINEFILE $EXE
"""

from collections import defaultdict
import pace.provbench.utilities as utilities

class Command:

    def __init__(self):
        """
        
        Local data members:
        launcher: e.g. mpirun, charmrun
        launcherargs: e.g. -np $NPROCESS -f $MACHINEFILE
        exe: application exetuable 
        exeargs: arguments for exetuable (e.g. <input.in)
        outputfile: output file after running (e.g. runtime.output)
        
        """
        self.launcher = ''
        self.launcherargs = ''
        self.exe = ''
        self.exeargs = ''
        self.outputfile = ''
        self.comm = ''


    def constructCommand(self, input):
        """
        This method builds the command to run the application, it bases on the recipe 
        provided by contributors, it updates self.comm to store the actual command
        
        :params input: recipe file passed from expInfo
        
        """
        
        
        self.myvars = utilities.parseFile(input,'=')
        if 'launcher' in self.myvars.keys():
            self.launcher = self.myvars['launcher'][0].strip()
        if 'launcherargs' in self.myvars.keys():
            self.launcherargs = self.myvars['launcherargs'][0].strip()
        if 'exe' in self.myvars.keys():
            self.exe = self.myvars['exe'][0].strip()
        if 'exeargs' in self.myvars.keys():
            self.exeargs = self.myvars['exeargs'][0].strip()
        if 'outputfile' in self.myvars.keys():
            self.outputfile = self.myvars['outputfile'][0].strip()

        self.comm = '(time ' + self.launcher + " " + self.launcherargs + " " + \
                    self.exe + " " + self.exeargs + ") 2>&1| tee -a " + \
                    self.outputfile  

    def getExe(self):
        """
        This method returns the exetuable name
        
        :return string 
        """
        return self.exe

    def getCommand(self):
        """
        This method returns the command line
        
        :return string
        """
        #replace the double quote to single quote
        #self.comm = self.comm.replace('"',"'")
        return self.comm

    def getOutputFileName(self):
        return self.outputfile
