#!/bin/tcsh
#PBS -N COMPILE_SMOON_004b
#PBS -lmem=12gb,nodes=1:ppn=1
#PBS -q jfl
#PBS -V
cd $PBS_O_WORKDIR

csh ./AVERAGING_SCRIPTS/SMOON_004b_average.csh
