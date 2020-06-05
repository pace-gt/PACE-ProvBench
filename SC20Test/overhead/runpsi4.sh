#/bin/bash
node=$1
workdir=$2
cd $workdir
module load pace/2020.01
module load psi4/1.3.2
MACHINEFILE=hosts.$node
NPROCS=`wc -l < ${MACHINEFILE}`
for i in {1..5};do
tempdir=run$i-psi4-$NPROCS+`date +%Y-%m-%d_%H_%M`
mkdir $tempdir 
cd $tempdir 
cp -r $workdir/../../Application/Input/psi4/* .
cp ../hosts.$node .
EXE=`which psi4`>> log.out
echo $EXE>>log.out
ldd $EXE >>log.out
echo md5 >>log.out
echo `which psi4|xargs md5sum|cut -d " " -f1` >>log.out
(time   psi4 input.dat -n$NPROCS) 2>&1| tee -a time.output
echo "Running success(0)/failed(1) "${PIPESTATUS[0]} >>log.out
cd ../ ;done
