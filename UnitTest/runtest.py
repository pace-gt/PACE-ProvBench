######################################################################
# Fang (Cherry) Liu 2019/04
#
# This the main entry point for PACE-ProvBench test
# before running this script, be sure you have prepared input.runtime
# and input.hosts, and copy the corresponding app files from allapps
# directory to apps directory
#
######################################################################



import pace.provbench.expinfo as expinfo
import os
import getopt, sys

def main():

    print("Running tests")

    fullCmdArguments = sys.argv
    argumentList = fullCmdArguments[1:]

    #print(argumentList)
    unixOptions = "a:i:n:p:"
    gnuOptions = ["app=","input=","nodes=", "ppn="]

    try:
        arguments, values = getopt.getopt(argumentList,unixOptions,gnuOptions)
    except getopt.error as err:
        print(str(err))
        sys.exit(2)

    for cArgument, cValue in arguments:
        if cArgument in ("-a", "--app"):
            appname = cValue
            print("Specifying application "+appname)
        elif cArgument in ("-i", "--input"):
            inputfile = cValue
            print("Specifying input file "+inputfile)
        elif cArgument in ("-n", "--nodes"):
            nodes = int(cValue)
            print("Specifying number of nodes for testing")
        elif cArgument in ("-p", "--ppn"):
            ppn = int(cValue)
            print("Specifying number of cores per node for testing")


    exp = expinfo.ExpInfo(appname, inputfile)
    exp.setNNodes(nodes)
    exp.setPPNs(ppn)
    exp.runExp()

    exp.dumpResult()

    exp.stop()
    return 0


if __name__ == '__main__':
    main()
