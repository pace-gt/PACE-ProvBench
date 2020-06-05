
for i in 1 2 4; do
(time python3 ../../UnitTest/runtest.py --app "lammps_rh7" --input "./input.runtime.rh7" --nodes $i --ppn 28) 2>&1| tee -a lammps.time.$i.28&
sleep 30


(time python3 ../../UnitTest/runtest.py --app "vasp" --input "./input.runtime.rh7" --nodes $i --ppn 28) 2>&1| tee -a vasp.time.$i.28& 
sleep 30


(time python3 ../../UnitTest/runtest.py --app "espresso_rh7" --input "./input.runtime.rh7" --nodes $i --ppn 28) 2>&1| tee -a espresso.time.$i.28
sleep 30
done
(time python3 ../../UnitTest/runtest.py --app "psi4_rh7" --input "./input.runtime.rh7" --nodes 1 --ppn 28) 2>&1| tee -a psi4.time.1.28&
sleep 30
