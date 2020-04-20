# -*- coding: utf-8 -*-
"""
Fang (Cherry) Liu (fang.liu@gatech.edu) 2019/10

SysInfo: 
This class caputures all system information for each individual host,
and it also ingests system data into the database
"""



import subprocess
import pace.provbench.database as database

class SysInfo:

    def __init__(self):
        """
        fileLocation : string, the location where output file syslog.user@hostname exists
        database: pace.provbench.database object, use for database ingestion
        """
        self.fileLocation = None
        self.database = database.DataBase

    def changeFileLocation(self,filepath):
        """
        This method modified the output file location
        :param filepath string
        """
        #assume the fname contains the full path
        self.fileLocation = filepath

    def dumpSystemInfo(self, userid, hostname, filepath, benchroot, database):
        """
        This method collectes system information for each host, and put the result both in 
        the file and database
        :param userid, string, who runs the experiments (has to have sudo permission)
        :param hostname, string, host the system information is collected
        :param filepath, string, where to store the system information
        :param database, database object
        """
        self.fileLocation = filepath
        #dump all system information into one single file
        connectName=userid+'@'+hostname
        output=filepath+'/syslog.'+connectName
        outfile=open(output,'w')
        command='sudo '+benchroot+'/Utilities/get_sysinfo.sh'
        try:
            ssh = subprocess.Popen(["ssh", "%s" % connectName, command],
                       shell=False,
                       stdout=subprocess.PIPE,
                       stderr=subprocess.PIPE)
            result = ssh.stdout.readlines()
        except:
            print('Connect to '+connectName+' failed, check your network and userid')
            exit(1)

        if result == []:
                error = ssh.stderr.readlines()
                print("ERROR: %s" % error)
        else:
                print(result)


        outfile.write(result[0].decode("utf-8"))
        hardwareinfo = result[0].decode("utf-8")
        outfile.close
        database.inputSystemInfo(hardwareinfo)

