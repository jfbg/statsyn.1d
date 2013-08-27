#!/bin/tcsh
#PBS -N COMPILE_BM_EARTHPREM_SPIKE_100km_dt05_CEES_B
#PBS -lmem=12gb,nodes=1:ppn=1
#PBS -q jfl
#PBS -V
cd $PBS_O_WORKDIR

csh ./AVERAGING_SCRIPTS/BM_EARTHPREM_SPIKE_100km_dt05_CEES_B_average.csh
