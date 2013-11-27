#!/bin/tcsh
#PBS -N COMPILE_BSMOON_005a
#PBS -lmem=12gb,nodes=1:ppn=1
#PBS -q jfl
#PBS -V
cd $PBS_O_WORKDIR

csh ./AVERAGING_SCRIPTS/BSMOON_005a_average.csh
