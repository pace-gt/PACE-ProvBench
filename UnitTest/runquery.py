######################################################################
# Fang (Cherry) Liu fang.liu@gatech.edu 2019/11
#
# This script queries provbench database using command line 
#
######################################################################



import pace.provbench.database as database
import pace.provbench.utilities as utilities
import os
import getopt, sys

def main():

    print("Query tests")

    fullCmdArguments = sys.argv
    argumentList = fullCmdArguments[1:]

    #print(argumentList)
    unixOptions = "f:d:o:r"
    gnuOptions = ["fields=","daterange=","output=","result="]

    try:
        arguments, values = getopt.getopt(argumentList,unixOptions,gnuOptions)
    except getopt.error as err:
        print(str(err))
        sys.exit(2)


    # construct the where query based on the user input
    # assume t1 is Experiments table
    #        t2 is Hosts table
    #        t3 is Modules table
    whereClause="WHERE "
    header = None
    output_file="result.csv"
    for cArgument, cValue in arguments:
        if cArgument in ("-f", "--fields"):
            fields = cValue
            print("query fields "+fields)
            whereClause += utilities.processQueryFields(fields) + " AND "
        elif cArgument in ("-d", "--daterange"):
            daterange = cValue.strip()
            whereClause +=  utilities.processQueryDate(daterange)
            print("query dates  "+daterange)
        elif cArgument in ("-o", "--output"):
            header = cValue.strip().split(",")
            print("return field  "+",".join(header))
        elif cArgument in ("-r", "--result"):
            output_file = cValue.strip().split(",")[0]
            print("output file  "+",".join(output_file))

    # remove the last AND from the string
    whereClause = whereClause.rsplit(' ',1)[0]

    db = database.DataBase()
    result = db.queryDB(whereClause,header)
    print(result)
    df = utilities.processQueryResult(result[1:],header)
    df.to_csv(output_file,sep='|')
    import subprocess
    command = "sed -i 's/\[//g' "+output_file
    subprocess.run(command, shell=True)
    command = "sed -i 's/\]//g' "+output_file
    subprocess.run(command, shell=True)
    command = 'sed -i "s/\'//g" '+output_file
    subprocess.run(command, shell=True)
    command = 'sed -i "s/\ //g" '+output_file
    subprocess.run(command, shell=True)
    db.close()


    return 0


if __name__ == '__main__':
    main()
