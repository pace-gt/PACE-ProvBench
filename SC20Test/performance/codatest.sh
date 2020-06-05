
for i in 1 2 4; do
(time python3 ../../UnitTest/runtest.py  --app "lammps_intel" --input "./input.runtime.coda" --nodes $i --ppn 28)2>&1| tee -a  lammps.$i.28.coda& 
sleep 30
(time python3 ../../UnitTest/runtest.py  --app "vasp" --input "./input.runtime.coda" --nodes $i --ppn 28)2>&1| tee -a  vasp.$i.28.coda&
sleep 30
(time python3 ../../UnitTest/runtest.py  --app "espresso" --input "./input.runtime.coda" --nodes $i --ppn 28)2>&1| tee -a  espresso.$i.28.coda &  
sleep 30
done
(time python3 ../../UnitTest/runtest.py  --app "lammps_intel" --input "./input.runtime.coda" --nodes 1 --ppn 40)2>&1| tee -a  lammps.1.40.coda& 
sleep 30
(time python3 ../../UnitTest/runtest.py  --app "vasp" --input "./input.runtime.coda" --nodes 1 --ppn 40)2>&1| tee -a  vasp.1.40.coda&
sleep 30
(time python3 ../../UnitTest/runtest.py  --app "espresso" --input "./input.runtime.coda" --nodes 1 --ppn 40)2>&1| tee -a  espresso.1.40.coda &  
sleep 30
(time python3 ../../UnitTest/runtest.py  --app "psi4" --input "./input.runtime.coda" --nodes 1 --ppn 28)2>&1| tee -a  psi4.1.28.coda& 
sleep 30
(time python3 ../../UnitTest/runtest.py  --app "psi4" --input "./input.runtime.coda" --nodes 1 --ppn 40)2>&1| tee -a  psi4.1.40.coda& 
sleep 30
