#!/bin/bash
source ../Utilities/bench_init.sh
python3 runtest.py --app "lammps_intel" --input "./input.runtime" --nodes 1 --ppn 8
python3 runtest.py --app "lammps_intel" --input "./input.runtime" --nodes 1 --ppn 32
python3 runtest.py --app "lammps_intel" --input "./input.runtime" --nodes 1 --ppn 40
python3 runtest.py --app "lammps_intel" --input "./input.runtime" --nodes 2 --ppn 32
python3 runtest.py --app "lammps_intel" --input "./input.runtime" --nodes 4 --ppn 32
