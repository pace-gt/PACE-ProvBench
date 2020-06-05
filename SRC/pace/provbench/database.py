# -*- coding: utf-8 -*-
"""
Fang (Cherry) Liu (fang.liu@gatech.edu) 2019/10

SysInfo: 
This class is the middle layer to the database operations 
"""


import time
import os
import mysql.connector
from mysql.connector import Error
import pace.provbench.utilities as utilities

class DataBase:

        # instance attributes
    def __init__(self):
        """
        local data members:
        
        db: database connector object 
        expTable: name of Experiments table in provbench database
        hostTable: name of Hosts table in provbench database
        moduleTable: name of Modules table in provbench database
        hostSchema: schema used to insert to Hosts table
        insertExpSchema: schema used to insert the Experiments table
        updateExpSchema: schema used to update the Experiments table
        moduleSchema: schema used to insert to Modules table
        expID: string, experiment ID
        """

        self.db = self.connect() 
        self.expTable = "Experiments"
        self.hostTable = "Hosts"
        self.moduleTable = "Modules"
        self.hostSchema = "(host_name,bios_version,system_manufacturer,system_product_name,system_version,system_serial_number,system_uuid,processor_family,processor_version,processor_frequency,cpus,sockets,total_memory_gb,ib_device,ib_rate,gpu_info,gpus,os_info,kernel,exp_id)"
        self.insertExpSchema = "(mode,software_name,software_version,scheduler,username,queue,nruns,total_nproc,ngpus)"               
        self.updateExpSchema =  "(md5,script,experiment_location,command,exit_status,pbs_jobid,average_time_seconds,maximum_time_seconds,minimum_time_seconds,standard_deviation,variance)"               
        self.moduleSchema = "(module,exp_id)"
        self.expID = None
        #print("init database")

    def connect(self):
        """
        This method establishes a database connection, and is only called once 
        and the handler is stored for furture reuse. 
        """
        try:
            connection = mysql.connector.connect(host="pbstools-coda.pace.gatech.edu",    # your host, usually localhost
                     user="provbench",         # your username
                     passwd="PACE-CODA!9",  # your password
                     db="provbench")        # name of the data base
            self.cursor = connection.cursor()
            if connection.is_connected():
                db_info = connection.get_server_info()
                #print("Connected to MariaDB Server version ", db_info)
            return connection

        except Error as e:
            print("Error while connecting to MariaDB Server",e)

    def close(self):
        """
        This method closes the database connection
        """
        print("Close the database connection")
        self.cursor.close()
        self.db.close()
        self.db = None
        self.cursor = None

    def insertExperiment(self, insertStr):
        """
        This method inserts a record to Experiments table and saves the expID 
        :param insertStr, string matches the oder in insertExpSchema
         e.g.("queue","leslie-spec","21Feb19","testflight-sched","fliu67","testflight","2","0","0")
        """
        sqlstatement = "INSERT INTO "+self.expTable+ " " +self.insertExpSchema + " VALUES "+ insertStr
        #print(sqlstatement)
        self.cursor = self.db.cursor()
        try:
            self.cursor.execute(sqlstatement)
            self.expID = self.cursor.lastrowid 
            #print("expID is ",self.expID)
            self.db.commit()
        except Exception as e:
            print("Failed to insert an experiment",e)
            exit(2)
        

    def updateExperiment(self, column, value):
        """
        This method update Experiments table with one field
        :param column string type, column in the schema
        :param value string type, input value
        """
        sqlstatement = "UPDATE "+ self.expTable + " SET "+column+"=\""+str(value)+"\" where exp_id=\""+str(self.expID)+"\""
        #print(sqlstatement)

        self.cursor = self.db.cursor()
        try:
            self.cursor.execute(sqlstatement)
            self.db.commit()
        except Exception as e:
            print("Failed to update an experiment",e)
            exit(2)
        
    def batchUpdateExperiment(self, columns, values):
        """
        This method updates Experiments table with multiple columns
        :param columns: a List with multiple fields in the schema
        :param values: a List with multiple input values corresponding to columns
        """

        updateStr = " SET "
        for column,value in zip(columns, values):
            updateStr += column + "=\""+str(value)
            if column != columns[-1]:
                updateStr += "\", "
            else:
                updateStr += "\""

        sqlstatement = "UPDATE "+ self.expTable + updateStr + " where exp_id=\""+str(self.expID)+"\""
        #sqlstatement = "INSERT INTO "+self.expTable+ " " +self.updateExpSchema + " VALUES "+ updateStr
        #print(sqlstatement)
        self.cursor = self.db.cursor()
        try:
            self.cursor.execute(sqlstatement)
            self.db.commit()
        except Exception as e:
            print("Failed to update an experiment",e)
            exit(2)
        
    def insertModule(self, insertStr):
        """
        This method inserts into Modules table
        :param inserStr is a List 
        """

        # construct the multiple insersion values
        insert =""
        for item in insertStr:
            insert += "(\""+item+"\",\""+str(self.expID)+"\")"
            if item != insertStr[-1]:
                insert += ", "
            else:
                insert += "; "

        sqlstatement = "INSERT INTO "+self.moduleTable+ " " +self.moduleSchema + " VALUES "+insert
                        
        #print(sqlstatement)
        self.cursor = self.db.cursor()
        try:
            self.cursor.execute(sqlstatement)
            self.db.commit()
        except Exception as e:
            print("Failed to insert an experiment",e)
            exit(2)



    def inputSystemInfo(self, hostinfo):
        """
        This method insert the hostinfo into provbench.Hosts table

        :param hostinfo: a string as 
        "compile-coda","R03","Penguin Computing","Relion XE2112GT","0100","P100208405","db938000-fd74-11e7-8000-e0d55e1aebe8","Xeon","Intel(R) Xeon(R) Gold 6226 CPU @ 2.70GHz","2700 MHz","24","2","192","mlx5_0","100","Tesla V100-PCIE-32GB","1","7.6","3.10.0-957.27.2.el7.x86_64"
        matches the provbench.Hosts table schema
        """
       
        #added the exp_id in the insert_str
                          

        insert_str = "("+hostinfo+" ,"+str(self.expID)+")"
        sqlstatement = "INSERT INTO " + self.hostTable + " " + self.hostSchema + " VALUES "+insert_str
        #print(sqlstatement)
        try:
            self.cursor.execute(sqlstatement)
            self.db.commit()
        except Exception as e:
            print("Failed to insert a host", e)
            exit(2)



    def queryDB(self, whereClause, header):
        """
        This method querys database
        :params whereClaus String
        :params header a list
        """
        
        
        
        experimentSchema = "mode,software_name,software_version,scheduler,username,queue,nodes,ppns,nruns,total_nproc,ngpus, md5,script,experiment_location,command,exit_status,pbs_jobid,average_time_seconds,maximum_time_seconds,minimum_time_seconds,standard_deviation,variance,exp_id"
        hostSchema = "host_name,bios_version,system_manufacturer,system_product_name,system_version,system_serial_number,system_uuid,processor_family,processor_version,processor_frequency,cpus,sockets,total_memory_gb,ib_device,ib_rate,gpu_info,gpus,os_info,kernel"
        moduleSchema = "module"

        if header != None:
            output = "SELECT t1.exp_id, t2.host_name, t3.module, t1.software_name, ";
            
            selection=" 'exp_id', 'host_name', 'module', 'software_name', "
            for item in header:            
                if item == 'exp_id' or item == 'host_name' or item == 'module' or item == 'software_name':
                    print("remove default fields")
                else:
                    selection += "\'"+item+"\' , "
                    if item in experimentSchema:
                        output += "t1."+item+" , "
                    elif item in hostSchema:
                        output += "t2."+item+" , "
                    elif item in moduleSchema:
                        output += "t3."+item+" , "
            output = output.rsplit(',',1)[0]
            output += " from Experiments as t1 join Hosts as t2 on t1.exp_id=t2.exp_id \
                        join Modules as t3 on t1.exp_id=t3.exp_id "+whereClause+";"
            selection = selection.rsplit(',',1)[0] 
            sqlstatement = " SELECT "+selection+" UNION "+output
        else:
            sqlstatement = "\
                        SELECT 'exp_id', 'exp_date', 'exp_time','mode','software_name', \
                        'software_version', 'scheduler','username','queue','pbs_jobid',\
                        'nruns','total_nproc','nodes','ppns','ngpus','experiment_location',\
                        'md5','command','exit_status',\
                        'average_time_seconds', 'standard_deviation', \
                         'maximum_time_seconds', 'minimum_time_seconds','variance', 'host_name', 'module'  \
                        UNION \
                        SELECT t1.exp_id, t1.exp_date, t1.exp_time,t1.mode,t1.software_name, \
                        t1.software_version, t1.scheduler, t1.username, t1.queue, t1.pbs_jobid,\
                        t1.nruns, t1.total_nproc, t1.nodes, t1.ppns, t1.ngpus, t1.experiment_location,\
                        t1.md5, t1.command, t1.exit_status, \
                        t1.average_time_seconds, t1.standard_deviation, \
                        t1.maximum_time_seconds, t1.minimum_time_seconds,t1.variance, t2.host_name, t3.module  \
                         from Experiments as t1 join Hosts as t2 on t1.exp_id=t2.exp_id \
                        join Modules as t3 on t1.exp_id=t3.exp_id "+whereClause+";"





        #print(sqlstatement)
        try:
            self.cursor.execute(sqlstatement)
            result = self.cursor.fetchall()
        except Exception as e:
            print("Fail to select from DB", e)
            exit(2)
        return result
