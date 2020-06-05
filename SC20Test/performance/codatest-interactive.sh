
for i in 1 2 4; do
echo "Test lammps by provbench interactively on " $i " nodes each with 28 cores" >>lammps.$i.28.interactive
(time python3 ../../UnitTest/runtest.py  --app "lammps_intel" --input "./input.runtime.interactive" --nodes $i --ppn 28)2>&1 >/dev/null| tee -a  lammps.$i.28.interactive
echo "Test vasp by provbench interactively on " $i " nodes each with 28 cores" >>vasp.$i.28.interactive
(time python3 ../../UnitTest/runtest.py  --app "vasp" --input "./input.runtime.interactive" --nodes $i --ppn 28)2>&1 >/dev/null| tee -a  vasp.$i.28.interactive
echo "Test espresso by provbench interactively on " $i " nodes each with 28 cores" >>espresso.$i.28.interactive
(time python3 ../../UnitTest/runtest.py  --app "espresso" --input "./input.runtime.interactive" --nodes $i --ppn 28)2>&1 >/dev/null| tee -a  espresso.$i.28.interactive   
done
echo "Test lammps by provbench interactively on 1 node with 40 cores" >>lammps.1.40.interactive
(time python3 ../../UnitTest/runtest.py  --app "lammps_intel" --input "./input.runtime.interactive" --nodes 1 --ppn 40)2>&1 >/dev/null| tee -a  lammps.1.40.interactive
echo "Test vasp by provbench interactively on 1 node with 40 cores" >>vasp.1.40.interactive
(time python3 ../../UnitTest/runtest.py  --app "vasp" --input "./input.runtime.interactive" --nodes 1 --ppn 40)2>&1 >/dev/null| tee -a  vasp.1.40.interactive
echo "Test espresso by provbench interactively on 1 node with 40 cores" >>espresso.1.40.interactive
(time python3 ../../UnitTest/runtest.py  --app "espresso" --input "./input.runtime.interactive" --nodes 1 --ppn 40)2>&1 >/dev/null| tee -a  espresso.1.40.interactive  
echo "Test psi4 by provbench interactively on 1 node with 28 cores" >>psi4.1.28.interactive
(time python3 ../../UnitTest/runtest.py  --app "psi4" --input "./input.runtime.interactive" --nodes 1 --ppn 28)2>&1 >/dev/null| tee -a  psi4.1.28.interactive
echo "Test psi4 by provbench interactively on 1 node with 40 cores" >>psi4.1.40.interactive
(time python3 ../../UnitTest/runtest.py  --app "psi4" --input "./input.runtime.interactive" --nodes 1 --ppn 40)2>&1 >/dev/null | tee -a  psi4.1.40.interactive
