######################################################################
# Fang (Cherry) Liu (fang.liu@gatech.edu) 2019/04
######################################################################

import pace.provbench.utilities as utilities
import os
import subprocess
import time
import numpy as np
class ResultInfo:

    def __init__(self):
        self.runtime = None
        self.result_time = [] #store the time for all runs in seconds in a list
        self.app = []
        self.header = ['Timestamp',
                       'Application Name',
                       'Version',
                       '# runs',
                       '# nodes',
                       '# ppn',
                       'Average Run time (seconds)',
                       'Maximum Run time (seconds)',
                       'Minimum Run time (seconds)',
                       'Run time Stand-deviation',
                        'Run time Variance',
                       'Hostlist',
                       'Success (0)/Failed(1)']

    def saveRuntime(self,runtime):
        self.runtime = runtime

    def saveApp(self,appinfo):
        self.app.append(appinfo)

    # this method can only run after saveRuntime and saveApp

    def parseResult(self,testdir,database):
        resultfile = open(testdir+'/summary.result', 'a')
        #print(self.app)
        # update columns in Experiments table
        # (md5,total_nproc,script,experiment_location,command,exit_status,pbs_jobid,average_time_seconds,maximum_time_seconds,minimum_time_seconds,standard_deviation,variance)
        expUpdateSchema=["total_nproc",
                    "nodes",
                    "ppns",
                    "exp_date",
                    "exp_time",
                    "experiment_location",
                    "command",
                    "exit_status",
                    "pbs_jobid",
                    "average_time_seconds",
                    "maximum_time_seconds",
                    "minimum_time_seconds",
                    "standard_deviation",
                    "variance"]

        outresult = [self.header]
        mode = self.runtime.getMode()

        #print("---------------- mode"+mode)
        for m in self.app:
            time_stamp = self.runtime.getTimeStamp()
            appname = str(m.getAppName())
            appver = str(m.getAppVersion())
            apptestdir = testdir+'/'+appname
            nruns = str(self.runtime.getRuns())
            nnodes = str(self.runtime.getNNodes())
            ppns = ','.join(self.runtime.getPPNs())
            self.analyzeTime(apptestdir)
            time_stat = self.getTimeStat()
            hosts = str(self.runtime.getHosts())
            success = str(self.getRunSuccess(apptestdir))
            nprocs = str(self.runtime.getTotalProcs())

            temp_result = [time_stamp,
                          appname,
                          appver,
                          nruns,
                          nnodes,
                          ppns,
                          str(time_stat[0]),
                          str(time_stat[1]),
                          str(time_stat[2]),
                          str(time_stat[3]),
                          str(time_stat[4]),
                          hosts,
                          success]

            if mode == 'iteractive':
                jobid = ""
            else:
                jobid = self.runtime.getJobIds(appname)
            valueToDB=[nprocs,
                       str(self.runtime.getNNodes()),
                       ",".join(self.runtime.getPPNs()),
                       self.runtime.getDate(),
                       self.runtime.getTime(),
                       apptestdir,
                       m.getCommand().replace('"',"'"),
                       success,
                       jobid,
                       str(time_stat[0]),
                       str(time_stat[1]),
                       str(time_stat[2]),
                       str(time_stat[3]),
                       str(time_stat[4])]
           

            database.batchUpdateExperiment(expUpdateSchema,valueToDB)
 
            #print(temp_result)
            outresult.append(temp_result)
            time.sleep(30)

        #print(outresult)
        utilities.formatResult(outresult,resultfile)

        resultfile.close()


    def getRunSuccess(self,apptestdir):
        # return 0 or 1
        # if one of test has result code as 1, all the tests failed

        sum_code = 0
        try:
            #grep the result return code per application, from log.out file

            command = 'grep -r \"^Running success\" '+apptestdir+'/*/log.out|cut -d \" \" -f3'
            result_code = list(os.popen(command).read())

            #remove the '\n', on odd position

            del result_code[1::2]

            #add up the result code
            for val in result_code:
                sum_code += int(val)


        except OSError:
            print('Failed to find log.out files under' + apptestdir)
            exit(2)
        else:
            return sum_code

    def analyzeTime(self,apptestdir):
        # find the average runtime (in seconds) given an application
        # assume very test dir has an output file named time.output
        # at the bottom  of the file:
        # real    62m13.330s
        # user    247m51.544s
        # sys     1m1.939s

        try:
            # grep the line starts with 'real'
            #checkexist = 'find '+apptestdir+' -iname time.output|wc -l'
            checkexist ='grep -r \"^real\" '+apptestdir+'/*/time.output|wc -l'
            nruns = self.runtime.getRuns()
            count = 0
            while count != nruns: 
                process = subprocess.Popen(checkexist, stdout=subprocess.PIPE, stderr=None, shell=True)
                count = int(process.communicate()[0].decode("utf-8"))
            command = 'grep -r \"^real\" '+apptestdir+'/*/time.output|awk \'{print $2}\''
            process = subprocess.Popen(command, stdout=subprocess.PIPE, stderr=None, shell=True)
            # output format is (b'62m17.822s\n62m13.330s\n', None)
			# convert the bytes to string
            output = process.communicate()[0].decode("utf-8")
            #print(output)
            # conver to list ['62m17.822s', '62m13.330s', '']
            run_time = output.split('\n')
            #print(run_time)
            # remove the last element ['62m17.822s', '62m13.330s']
            run_time =  run_time[:len(run_time)-1]
            #print(len(run_time))
            # sum all tests in run time
            sum_time = 0
            for each_time in run_time:
                each_time_seconds = utilities.convertTimeToSeconds(each_time)
                self.result_time.append(each_time_seconds) 
                


        except OSError:
            print('Failed to find time.output files under' + apptestdir)
            exit(2)
        
           

    def getTimeStat(self):
        time_ave = np.average(self.result_time)
        time_max = max(self.result_time)
        time_min = min(self.result_time)
        time_std = np.std(self.result_time, dtype = np.float32)
        time_var = np.var(self.result_time)

        return [time_ave, time_max, time_min, time_std, time_var]

"""
    def constructDBUpdateString(self):
        
        This method update the all performance data for each experiment to the database
        it constructs a string with key1=value1,key2=value2 ...
           

        setString = "compiler= "+self.compiler+","\
                    "mpi="+self.mpi+"," \
                    "dependlibs="+self.dependlibs+"," \
                    "md5="+self.appinfo.generateHashcode()+","\
                    "total_nproc"+self.,script,experiment_location,command,exit_status,pbs_jobid,average_time_seconds,maximum_time_seconds,minimum_time_seconds,standard_deviation,variance
"""        
