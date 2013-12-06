#!/bin/tcsh
#PBS -N COMPILE_PVPREM_002
#PBS -lmem=12gb,nodes=1:ppn=1
#PBS -q jfl
#PBS -V
cd $PBS_O_WORKDIR

csh ./AVERAGING_SCRIPTS/PVPREM_002_average.csh
