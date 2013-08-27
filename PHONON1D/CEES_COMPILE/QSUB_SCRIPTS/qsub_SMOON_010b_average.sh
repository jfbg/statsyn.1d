#!/bin/tcsh
#PBS -N COMPILE_SMOON_010b
#PBS -lmem=12gb,nodes=1:ppn=1
#PBS -q jfl
#PBS -V
cd $PBS_O_WORKDIR

csh ./AVERAGING_SCRIPTS/SMOON_010b_average.csh
