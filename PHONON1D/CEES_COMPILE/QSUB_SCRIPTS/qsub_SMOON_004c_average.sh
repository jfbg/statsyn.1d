#!/bin/tcsh
#PBS -N COMPILE_SMOON_004c
#PBS -lmem=12gb,nodes=1:ppn=1
#PBS -q jfl
#PBS -V
cd $PBS_O_WORKDIR

csh ./AVERAGING_SCRIPTS/SMOON_004c_average.csh
