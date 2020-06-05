#/bin/bash
# Fang (Cherry) Liu 2020/06/03
# This script is to run the test without PACE-Provbench
# run as run-interactive.sh 1 `pwd`
node=$1
PWD=$2
cd $2
(time bash runlammps.sh $node $PWD )2>&1 | tee -a lammps.$node.28.interactive
(time bash runpsi4.sh $node $PWD )2>&1 | tee -a psi4.$node.28.int$noderactive

(time bash runespresso.sh $node $PWD )2>&1 | tee -a espresso.$node.28.interactive
(time bash runvasp.sh $node $PWD )2>&1 | tee -a vasp.$node.28.interactive
