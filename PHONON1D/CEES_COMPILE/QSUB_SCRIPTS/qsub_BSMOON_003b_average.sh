#!/bin/tcsh
#PBS -N COMPILE_BSMOON_003b
#PBS -lmem=12gb,nodes=1:ppn=1
#PBS -q jfl
#PBS -V
cd $PBS_O_WORKDIR

csh ./AVERAGING_SCRIPTS/BSMOON_003b_average.csh
