#/bin/bash
node=$1
workdir=$2
cd $workdir
module load intel/19.0.5
module load mvapich2/2.3.2
module load lammps/09Jan20
MACHINEFILE=hosts.$node
NPROCS=`wc -l < ${MACHINEFILE}`

for i in {1..5};do
tempdir=run$i-lammps-$NPROCS+`date +%Y-%m-%d_%H_%M`
mkdir $tempdir 
cd $tempdir 
cp -r $workdir/../../Application/Input/lammps/* .
cp ../hosts.$node .
EXE=`which lmp`>> log.out
echo $EXE>>log.out
ldd $EXE >>log.out
echo md5 >>log.out
echo `which lmp|xargs md5sum|cut -d " " -f1` >>log.out
(time mpirun -np $NPROCS -machinefile $MACHINEFILE lmp < in.chute) 2>&1| tee -a time.output
echo "Running success(0)/failed(1) "${PIPESTATUS[0]} >>log.out
cd ../ ;done
