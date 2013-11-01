#!/bin/tcsh
#PBS -N COMPILE_CSMOON_011a_noscat
#PBS -lmem=12gb,nodes=1:ppn=1
#PBS -q jfl
#PBS -V
cd $PBS_O_WORKDIR

csh ./AVERAGING_SCRIPTS/CSMOON_011a_noscat_average.csh
