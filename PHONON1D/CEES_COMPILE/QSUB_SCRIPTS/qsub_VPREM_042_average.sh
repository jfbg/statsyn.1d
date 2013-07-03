#!/bin/tcsh
#PBS -N COMPILE_VPREM_042
#PBS -lmem=12gb,nodes=1:ppn=1
#PBS -q jfl
#PBS -V
cd $PBS_O_WORKDIR

csh ./AVERAGING_SCRIPTS/VPREM_042_average.csh
