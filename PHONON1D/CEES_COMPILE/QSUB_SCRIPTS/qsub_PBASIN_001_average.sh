#!/bin/tcsh
#PBS -N COMPILE_PBASIN_001
#PBS -lmem=12gb,nodes=1:ppn=1
#PBS -q jfl
#PBS -V
cd $PBS_O_WORKDIR

csh ./AVERAGING_SCRIPTS/PBASIN_001_average.csh
