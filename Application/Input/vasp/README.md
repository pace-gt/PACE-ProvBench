
# Application: vasp/5.4.4

[Vasp](https://www.vasp.at/) The Vienna Ab initio Simulation Package (VASP) is a computer program for atomic scale materials modelling, e.g. electronic structure calculations and quantum-mechanical molecular dynamics, from first principles.

VASP computes an approximate solution to the many-body Schrödinger equation, either within density functional theory (DFT), solving the Kohn-Sham equations, or within the Hartree-Fock (HF) approximation, solving the Roothaan equations. Hybrid functionals that mix the Hartree-Fock approach with density functional theory are implemented as well. Furthermore, Green's functions methods (GW quasiparticles, and ACFDT-RPA) and many-body perturbation theory (2nd-order Møller-Plesset) are available in VASP.

In VASP, central quantities, like the one-electron orbitals, the electronic charge density, and the local potential are expressed in plane wave basis sets. The interactions between the electrons and ions are described using norm-conserving or ultrasoft pseudopotentials, or the projector-augmented-wave method.

To determine the electronic groundstate, VASP makes use of efficient iterative matrix diagonalisation techniques, like the residual minimisation method with direct inversion of the iterative subspace (RMM-DIIS) or blocked Davidson algorithms. These are coupled to highly efficient Broyden and Pulay density mixing schemes to speed up the self-consistency cycle.

## Version/Compiler/Libs

* Version: 5.4.4
* Compiler: intel/19.0 mvapich2/2.3
* Libs: mkl/2019u1 cuda/10.0

module file located at Applications/module/vasp/5.4.4.lua


## Test case
testcase PbO2 from https://www.nsc.liu.se/~pla/vasptest/


### Software locations
Source
```
/gpfs/pace1/project/pace-admins/Benchmarks/CascadeLake/Software/vasp-5.4.4
```
Installation
```
/gpfs/pace1/project/pace-admins/Benchmarks/CascadeLake/Software/Install/vasp
```

### Input locations
```
Applications/source/vasp/
```
### Run the script
```
cd PACE-Bench/Test
./mri_test.sh `pwd`/../Applications `pwd`/common/machines 1 28 vasp
```

### test result
Output file time.output
This test runs on rich133-s40-17-skylake with 28 cored
```
rich133-s40-17-skylake.pace.gatech.edu+1+28+20190415_17_49_49/vasp/28+20190415_17_49_49
```
```
 LDA part: xc-table for Pade appr. of Perdew
 POSCAR, INCAR and KPOINTS ok, starting setup
 FFT: planning ...
 WAVECAR not read
 entering main loop
       N       E                     dE             d eps       ncg     rms          rms(c)
DAV:   1     0.142120488531E+04    0.14212E+04   -0.42402E+04  7356   0.161E+03
DAV:   2     0.176133601687E+03   -0.12451E+04   -0.11772E+04  7944   0.494E+02
DAV:   3    -0.541898615317E+02   -0.23032E+03   -0.22215E+03  8416   0.237E+02
DAV:   4    -0.706903731597E+02   -0.16501E+02   -0.16311E+02 10016   0.666E+01
DAV:   5    -0.710353278316E+02   -0.34495E+00   -0.34410E+00  9424   0.103E+01    0.269E+01
DAV:   6    -0.624587190929E+02    0.85766E+01   -0.37109E+01 10896   0.530E+01    0.722E+00
DAV:   7    -0.632206996075E+02   -0.76198E+00   -0.44215E+00  8412   0.994E+00    0.409E+00
DAV:   8    -0.631889539737E+02    0.31746E-01   -0.37548E-01 11296   0.387E+00    0.178E+00
DAV:   9    -0.631736658385E+02    0.15288E-01   -0.48943E-02 10876   0.173E+00    0.483E-01
DAV:  10    -0.631733511667E+02    0.31467E-03   -0.17846E-02 11480   0.937E-01    0.184E-01
DAV:  11    -0.631738045060E+02   -0.45334E-03   -0.30328E-03 10408   0.339E-01    0.103E-01
DAV:  12    -0.631741242090E+02   -0.31970E-03   -0.43752E-04  9968   0.174E-01    0.381E-02
DAV:  13    -0.631742245086E+02   -0.10030E-03   -0.13431E-04  8656   0.633E-02    0.209E-02
DAV:  14    -0.631742237719E+02    0.73668E-06   -0.18886E-05  6004   0.383E-02
   1 F= -.63174224E+02 E0= -.63174224E+02  d E =0.000000E+00  mag=    -0.0000
 writing wavefunctions

real    2m39.816s
user    37m18.394s
sys     31m22.215s
```

### test time
in the output file named time.output
```
real    2m39.816s
user    37m18.394s
sys     31m22.215s
```


* **Fang (Cherry) Liu** - *Initial work* 
