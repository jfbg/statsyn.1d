#!/bin/tcsh
#PBS -N COMPILE_CSMOON_001_noscat_OnlyPS_B
#PBS -lmem=12gb,nodes=1:ppn=1
#PBS -q jfl
#PBS -V
cd $PBS_O_WORKDIR

csh ./AVERAGING_SCRIPTS/CSMOON_001_noscat_OnlyPS_B_average.csh
