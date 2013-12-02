#!/bin/tcsh
#PBS -N COMPILE_RSMOON_011a
#PBS -lmem=12gb,nodes=1:ppn=1
#PBS -q jfl
#PBS -V
cd $PBS_O_WORKDIR

csh ./AVERAGING_SCRIPTS/RSMOON_011a_average.csh
