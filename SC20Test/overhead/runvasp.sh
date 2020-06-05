#/bin/bash
node=$1
workdir=$2
module load vasp/5.4.4
MACHINEFILE=hosts.$node
NPROCS=`wc -l < ${MACHINEFILE}`
for i in {1..5};do
tempdir=run$i-vasp-$NPROCS+`date +%Y-%m-%d_%H_%M`
mkdir $tempdir 
cd $tempdir 
cp -r $workdir/../../Application/Input/vasp/* .
cp ../hosts.$node .
EXE=`which vasp`>> log.out
echo $EXE>>log.out
ldd $EXE >>log.out
echo md5 >>log.out
echo `which vasp|xargs md5sum|cut -d " " -f1` >>log.out
(time mpirun -np $NPROCS -machinefile $MACHINEFILE vasp ) 2>&1| tee -a time.output
echo "Running success(0)/failed(1) "${PIPESTATUS[0]} >>log.out
cd ../ ;done
