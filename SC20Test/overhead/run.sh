#/bin/bash
# Fang (Cherry) Liu 2020/06/03
# This script is to run the test without PACE-Provbench


for i in {1..4}; do

lammps_jobid=$(qsub runlammps.pbs -l nodes=$i:ppn=28)
echo lammps $i node test with job $lammps_jobid >> overhead.test

psi4_jobid=$(qsub runpsi4.pbs -l nodes=$i:ppn=28)
echo psi4 $i node test with job $psi4_jobid >> overhead.test


vasp_jobid=$(qsub runvasp.pbs -l nodes=$i:ppn=28)
echo vasp $i node test with job $vasp_jobid >> overhead.test

espresso_jobid=$(qsub runespresso.pbs -l nodes=$i:ppn=28)
echo espresso $i node test with job $espresso_jobid >> overhead.test
done
