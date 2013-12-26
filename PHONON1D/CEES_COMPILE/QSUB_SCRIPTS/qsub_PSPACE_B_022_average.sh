#!/bin/tcsh
#PBS -N COMPILE_PSPACE_B_022
#PBS -lmem=12gb,nodes=1:ppn=1
#PBS -q jfl
#PBS -V
cd $PBS_O_WORKDIR

csh ./AVERAGING_SCRIPTS/PSPACE_B_022_average.csh
