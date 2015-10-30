#!/bin/tcsh
#PBS -N COMPILE_PBASIN_2601
#PBS -lmem=12gb,nodes=1:ppn=1
#PBS -q jfl
#PBS -V
cd $PBS_O_WORKDIR

csh ./AVERAGING_SCRIPTS/PBASIN_2601_average.csh
