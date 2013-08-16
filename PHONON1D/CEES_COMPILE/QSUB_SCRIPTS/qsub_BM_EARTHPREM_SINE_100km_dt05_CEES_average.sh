#!/bin/tcsh
#PBS -N COMPILE_BM_EARTHPREM_SINE_100km_dt05_CEES
#PBS -lmem=12gb,nodes=1:ppn=1
#PBS -q jfl
#PBS -V
cd $PBS_O_WORKDIR

csh ./AVERAGING_SCRIPTS/BM_EARTHPREM_SINE_100km_dt05_CEES_average.csh
