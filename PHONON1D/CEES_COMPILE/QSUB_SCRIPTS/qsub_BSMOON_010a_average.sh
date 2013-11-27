#!/bin/tcsh
#PBS -N COMPILE_BSMOON_010a
#PBS -lmem=12gb,nodes=1:ppn=1
#PBS -q jfl
#PBS -V
cd $PBS_O_WORKDIR

csh ./AVERAGING_SCRIPTS/BSMOON_010a_average.csh
