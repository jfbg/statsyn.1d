#!/bin/tcsh
#PBS -N COMPILE_PBASIN_2602
#PBS -lmem=12gb,nodes=1:ppn=1
#PBS -q jfl
#PBS -V
cd $PBS_O_WORKDIR

csh ./AVERAGING_SCRIPTS/PBASIN_2602_average.csh
