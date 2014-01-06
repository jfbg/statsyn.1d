#!/bin/tcsh
#PBS -N COMPILE_CSIMPLEMOON_Q6500
#PBS -lmem=12gb,nodes=1:ppn=1
#PBS -q jfl
#PBS -V
cd $PBS_O_WORKDIR

csh ./AVERAGING_SCRIPTS/CSIMPLEMOON_Q6500_average.csh
