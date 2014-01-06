#!/bin/tcsh
#PBS -N COMPILE_CSIMPLEMOON_vlvl
#PBS -lmem=12gb,nodes=1:ppn=1
#PBS -q jfl
#PBS -V
cd $PBS_O_WORKDIR

csh ./AVERAGING_SCRIPTS/CSIMPLEMOON_vlvl_average.csh
