#!/usr/bin/bash
# Fang (Cherry) Liu fang.liu@gatech.edu 2019/11
# this script is built into PBS script by pace/provbench/runtimeinfo.py
# This script converts the nodefile to machine.list
# in the format of:
# host1:ppn1
# host2:ppn2
# ...
#if [[ $# -ne 1 ]]; then
#    echo "Please specify an input file path"
#    exit
#fi

# assume nodefile exists at the current direcotry:
# multiple lines with each host per line, host can be replicated
# host1
# host1
# host2

rm machine.list
uniqmachine=$(sort nodefile|uniq)
for machine in $uniqmachine; do
    count=$(grep $machine nodefile|wc -l)
    echo $machine":"$count>>machine.list

done

