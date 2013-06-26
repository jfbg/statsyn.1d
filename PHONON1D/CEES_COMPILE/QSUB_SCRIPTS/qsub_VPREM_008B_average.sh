#!/bin/tcsh
#PBS -N COMPILE_VPREM_008B
#PBS -l nodes=1:ppn=1, mem=12gb
#PBS -q jfl
#PBS -V
cd $PBS_O_WORKDIR

csh ./AVERAGING_SCRIPTS/VPREM_008B_average.csh
