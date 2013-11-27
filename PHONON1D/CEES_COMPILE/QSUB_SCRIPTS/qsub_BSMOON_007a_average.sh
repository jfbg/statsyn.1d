#!/bin/tcsh
#PBS -N COMPILE_BSMOON_007a
#PBS -lmem=12gb,nodes=1:ppn=1
#PBS -q jfl
#PBS -V
cd $PBS_O_WORKDIR

csh ./AVERAGING_SCRIPTS/BSMOON_007a_average.csh
