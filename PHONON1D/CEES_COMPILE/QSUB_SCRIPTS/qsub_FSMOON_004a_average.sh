#!/bin/tcsh
#PBS -N COMPILE_FSMOON_004a
#PBS -lmem=12gb,nodes=1:ppn=1
#PBS -q jfl
#PBS -V
cd $PBS_O_WORKDIR

csh ./AVERAGING_SCRIPTS/FSMOON_004a_average.csh
