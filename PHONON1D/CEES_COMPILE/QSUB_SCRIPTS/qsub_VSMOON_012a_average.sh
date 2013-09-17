#!/bin/tcsh
#PBS -N COMPILE_VSMOON_012a
#PBS -lmem=12gb,nodes=1:ppn=1
#PBS -q jfl
#PBS -V
cd $PBS_O_WORKDIR

csh ./AVERAGING_SCRIPTS/VSMOON_012a_average.csh
