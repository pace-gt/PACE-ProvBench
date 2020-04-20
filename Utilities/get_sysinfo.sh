#!/bin/bash
# This script is called by pace/provbench/sysinfo.py, to collect the system information
# needs to run by sudo user 
# mehmet.belgin@oit.gatech.edu
# fang.liu@gatech.edu


if [[ $EUID -ne 0 ]]; then
	echo "This script must be run as root" 
	exit 1
fi

hn=$(hostname -s)
#system
biover=$(dmidecode -s bios-version) #R03
sysman=$(dmidecode -s system-manufacturer) #Penguin Computing
syspro=$(dmidecode -s system-product-name) #Relion XE2112GT
sysver=$(dmidecode -s system-version) #0100
sysser=$(dmidecode -s system-serial-number) #P100187763
sysuuid=$(dmidecode -s system-uuid) #db938000-fd74-11e7-8000-e0d55e1aebe8
profam=$(dmidecode -s processor-family|head -n 1) #Xeon Xeon
prover=$(dmidecode -s processor-version| head -n 1) #Intel(R) Xeon(R) Gold 6226 CPU @ 2.70GHz Intel(R) Xeon(R) Gold 6226 CPU @ 2.70GHz
profre=$(dmidecode -s processor-frequency| head -n 1) #2700 MHz 2700 MHz

#cpus
sockets=$(lscpu | grep -i '^socket'|cut -d ":" -f2|xargs) #2
cores=$(lscpu | grep '^CPU(s):'|cut -d ":" -f2|xargs) #24

#memory
memgb=$(dmidecode -t memory | grep -i Size:  | egrep -v "None|kB|Installed" | awk '{if ($3 == "MB") {print $2} else if ($3 == "GB") {print $2*1024} else {print "NA"}}' | uniq -c|awk '{print $1*$2/1024}')


#ib info
ibdev=$(ibstat --list_of_cas) #mlx5_0
ibrate=$(ibstat | grep Rate|cut -d ":" -f2|xargs) #100

#gpu info
gputype=$(nvidia-smi --list-gpus|head -n 1| cut -d ":" -f2|cut -d"(" -f1|xargs) # Tesla V100-PCIE-32GB
gpus=$(nvidia-smi --list-gpus|wc -l) #1

#os info
osver=$(cat /etc/redhat-release | awk '{print $(NF-1)}') #6.7
kernel=$(uname -r) #2.6.32-573.12.1.el6.x86_64

# matches the provbench.Hosts table schema
echo "\"$hn\",\"$biover\",\"$sysman\",\"$syspro\",\"$sysver\",\"$sysser\",\"$sysuuid\",\"$profam\",\"$prover\",\"$profre\",\"$cores\",\"$sockets\",\"$memgb\",\"$ibdev\",\"$ibrate\",\"$gputype\",\"$gpus\",\"$osver\",\"$kernel\""

