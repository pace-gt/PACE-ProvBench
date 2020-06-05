######################################################################
# Fang (Cherry) Liu (fang.liu@gatech.edu) 2019/04
######################################################################
from collections import defaultdict
import time
import datetime
import os

def isFileExist(filename):
    #to check if the file exists or not, return a file handler if success
    try:
        file = open(filename)
    except IOError as e:
        print('I/O error({0}:{1})'.format(e.errno, e.strerror))
        print(filename + " doesn't exist")
        exit(2)
    return file

# parseFile: function to parse a key<delim>value per line file
# args: 
#   filename : inputfile
#   delim : deliminator
# return:
#   myvars: a dictionary {key1:value1, key2:[value21,value22,..], ..}
def parseFile(filename, delim):
    #given a filename, return a dictionary
    file = isFileExist(filename)
    myvars = defaultdict(list)
    for fline in file:
        line = fline.strip()
        # remove the comments and block [Application] etc
        if not line.startswith('#') and not line.startswith('['):
            keyname, var = line.strip().partition(delim)[::2]
            myvars[keyname.strip()].append(var.strip())

    return myvars

def getTimeStamp():
    #format current timestamp in 20190327_17_17_58
    currentTime = time.time()
    formatTime = datetime.datetime.fromtimestamp(currentTime).strftime('%Y%m%d_%H_%M_%S')
    mdate = datetime.datetime.fromtimestamp(currentTime).strftime('%Y%m%d')
    #mtime = datetime.datetime.fromtimestamp(currentTime).strftime('%H_%M_%S')
    mtime = int(currentTime)
    return formatTime,mdate,mtime

def createDir(dirpath):
    if os.path.exists(dirpath):
        return dirpath
    else:

        try:
            os.makedirs(dirpath)
        except OSError:
            print('Creation of the directory %s failed' % dirpath)
            exit(2)
        else:
            print('Successfully created the directory %s' % dirpath)
        return dirpath

def getRepoRootDir():
    try:
        rootDir = os.popen('git rev-parse --show-toplevel').read()
    except OSError:
        print('Failed to get repository root directory %s, are you sure you are in-side a repository' % rootDir)
        exit(2)
    else:
        return rootDir.strip('\n')

def copyDir(sourcedir,destdir):
    try:
        command = 'cp -r '+sourcedir+'/* '+destdir
        os.popen(command)
    except OSError:
        print('Failed to copy '+sourcedir)
        exit(2)
    else:
        print('Successfully copied from '+sourcedir+' to '+destdir)

def countFileLen(file):
    try:
        command = 'wc -l <'+file
        count = int(os.popen(command).read())
    except OSError:
        print('Failed to open the file to count '+file)
        exit(3)
    else:
        return count

def getBenchRootDir():
	# find the parent directory, assume running on UnitTest directory
    # also setup the environmental varialbe 
    try:
        parent_path = os.path.abspath('..')
        os.environ['PACEPROVBENCHROOT'] = parent_path
    except OSError:
        print('Failed to find the parent path')
        exit(2)
    else:
        return parent_path


def convertTimeToSeconds(timestr):
    # this routine converts '62m17.822s' into  3737.822 seconds

    # first parse into minutes
    split1 = timestr.split('m')
    mins = float(split1[0])

    # second parse seconds
    split2 = split1[1].split('s')
    seconds = float(split2[0])

    total_seconds = mins*60 + seconds

    return total_seconds

def formatResult(data, result_file):
    # this routine prints result report
    # data is a double list, the first element is header
    # result_file is a file object to output

    dash = '-' * 130
    for i in range(len(data)):
        if i == 0:
            print(dash,file=result_file)
            print('{:<12s}|{:>12s}|{:>12s}|{:>12s}|{:>12s}|{:>12s}|{:>20s}|{:>20s}|{:>20s}|{:>20s}|{:>20s}|{:>20s}|{:>4s}|'.format(data[i][0],
                                                                                   data[i][1],
                                                                                   data[i][2],
                                                                                   data[i][3],
                                                                                   data[i][4],
                                                                                   data[i][5],
                                                                                   data[i][6],
                                                                                   data[i][7],
                                                                                   data[i][8],
                                                                                   data[i][9],
                                                                                   data[i][10],
                                                                                   data[i][11],
                                                                                   data[i][12]),file=result_file)
            print(dash,file=result_file)
        else:
            print(data[i][7])
            print('{:<12s}|{:>12s}|{:>12s}|{:>12s}|{:>12s}|{:>12s}|{:>20s}|{:>20s}|{:>20s}|{:>20s}|{:>20s}|{:>20s}|{:>4s}'.format(data[i][0],
                                                                                  data[i][1],
                                                                                  data[i][2],
                                                                                  data[i][3],
                                                                                  data[i][4],
                                                                                  data[i][5],
                                                                                  data[i][6],
                                                                                  data[i][7],
                                                                                  data[i][8],
                                                                                  data[i][9],
                                                                                  data[i][10],
                                                                                  data[i][11],
                                                                                  data[i][12]), 
            file=result_file)



def parseLogInfo(filename):
    """
    This method parses the log.out in the experiment directory, and returns 
    module list, md5 code for the application, and hostlist
    The file format is expected:
    
    Modules:
    Currently Loaded Modules:
    1) intel/19.0.3/mvapich2/2.3.1   2) leslie-spec/21Feb19
    MD5: 
    857daf02dc8f0c6284fcce20ee6f17c0
    Hostlist:N
    host1
    host2

    :params filename: the absolute path for the file <pull path>/log.out

    :return: output : a defaultdict with {'Modules':[module1, module2, ...], 
                                 'MD5':[857daf02dc8f0c6284fcce20ee6f17c0],
                                 'Hostlist':[host1, host2, ...]
                                 }

    """


    
    file = isFileExist(filename)
    output = defaultdict(list)
    for fline in file:
        line = fline.strip()
        if line.startswith("md5"):
            """
            md5 only has one line after the keyword
            """
            keyname = line.strip()
            var = next(file)
            output[keyname].append(var.strip())
        elif line.startswith('hostlist') or line.startswith('modules'):
            """
            both hostlist and modules have multiple lines after the keyword:counts line
            """
            keyname, nodes = line.strip().partition(":")[::2]
            for i in range(int(nodes)):
                var = next(file) 
                output[keyname.strip()].append(var.strip())        

    return output

def processQueryFields(queryString):
    """
    This function parses  & or | separated sstring
    :params queryString: e.g. software_name:<app name>&host:<host name>&total_nproc:24

    :return a whereClause for select query 
    """

    import re
    command = queryString.replace('&', ' AND ')
    command = command.replace('|', ' OR ')
    command = command.replace(':','=')

    if 'host_name' in command:
        command = command.replace('host_name=','t2.host_name like ')
    if 'module' in command:
        command = command.replace('module=','t3.module like ')

    print(command)
    return command

def processQueryDate(dateRange):

    """
    This method queries the date range in a format 20191114-20191115
    :params dateRange string 
    :return partial query statement
    """

    import re
        
    start_date, end_date = re.split('-',dateRange)

    returnstr = " t1.exp_date >= '"+start_date+"' AND t1.exp_date <= '"+end_date+"' AND"


    return returnstr
        

    
def processQueryResult(queryResult, header):
    """
    This function takes queryResult from runQuery, and futher parsing it into 
    uniq line:
    exp_id, exp_date, [host1, host2], [module1,module2] ...
      
    :params queryResult a list
    :return a dataframe
    """

    if header == None:

        header_new =['exp_id', 'exp_date', 'exp_time','mode','software_name', \
                        'software_version', 'scheduler','username','queue','pbs_jobid',\
                        'nruns','total_nproc','nodes','ppns','ngpus','experiment_location',\
                        'md5','command','exit_status',\
                        'average_time_seconds', 'standard_deviation', \
                         'maximum_time_seconds', 'minimum_time_seconds','variance', 'host_name', 'module' ]
    else:
        #make sure don't duplicate the default field if user specifies it from command line
        header_default=['exp_id', 'host_name', 'module','software_name']
        header_new=header_default
        for item in header:
            if item not in header_default:
                header_new.append(item)

        
        print(' '.join(header_new))

    import pandas as pd
    df = pd.DataFrame(queryResult, columns=header_new)
    # aggregate based on host name and module, and put them in the list format
    df_hm = df.groupby(df['exp_id']).agg({'host_name':lambda tdf: tdf.unique().tolist(),\
                                        'module':lambda tdf: tdf.unique().tolist(),\
                                        'software_name':lambda tdf: tdf.unique().tolist()})
    #print(df_hm)
    # remove duplicated rows due to multiple hosts and modules
    df_new = df.drop(['host_name','module','software_name'], axis=1)
    df_new = df_new.drop_duplicates()
    #print(df_new)
    # combine the aggregated host and module with rest of output fields 
    df_result = pd.merge(df_hm, df_new, how='left', on = 'exp_id')
    #print(df_result)
    return df_result
