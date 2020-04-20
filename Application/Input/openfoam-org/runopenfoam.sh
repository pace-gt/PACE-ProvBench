#!/bin/bash

# load openfoam modules and source etc files
#module load pace/2020.01; module load gcc/8.3.0 openfoam-org/5.0-mva2
source $OPENFOAM_ORG_ROOT/etc/bashrc

MV2_ENABLE_AFFINITY=0 mpiexec -n 4 interFoam -parallel
