#!/bin/tcsh
#PBS -N COMPILE_BM_onlySPIKE
#PBS -lmem=12gb,nodes=1:ppn=1
#PBS -q jfl
#PBS -V
cd $PBS_O_WORKDIR

csh ./AVERAGING_SCRIPTS/BM_onlySPIKE_average.csh
