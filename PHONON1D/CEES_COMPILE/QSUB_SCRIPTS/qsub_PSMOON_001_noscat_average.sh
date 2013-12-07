#!/bin/tcsh
#PBS -N COMPILE_PSMOON_001_noscat
#PBS -lmem=12gb,nodes=1:ppn=1
#PBS -q jfl
#PBS -V
cd $PBS_O_WORKDIR

csh ./AVERAGING_SCRIPTS/PSMOON_001_noscat_average.csh
