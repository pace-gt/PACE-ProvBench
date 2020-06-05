#/bin/bash
node=$1
workdir=$2
cd $workdir
module load intel/19.0.5
module load mvapich2/2.3.2
module load espresso/6.4
MACHINEFILE=hosts.$node
NPROCS=`wc -l < ${MACHINEFILE}`
for i in {1..5};do
tempdir=run$i-espresso-$NPROCS+`date +%Y-%m-%d_%H_%M`
mkdir $tempdir 
cd $tempdir 
cp -r $workdir/../../Application/Input/espresso/* .
cp ../hosts.$node .
EXE=`which pw.x`>> log.out
echo $EXE>>log.out
ldd $EXE >>log.out
echo md5 >>log.out
echo `which pw.x|xargs md5sum|cut -d " " -f1` >>log.out
(time mpirun -np $NPROCS -machinefile $MACHINEFILE pw.x -i ausurf.in > file.out) 2>&1| tee -a time.output
echo "Running success(0)/failed(1) "${PIPESTATUS[0]} >>log.out
cd ../ ;done
