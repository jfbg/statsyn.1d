#!/bin/tcsh
#PBS -N COMPILE_VPREM_022
#PBS -l nodes=1:ppn=1, mem=12gb
#PBS -q jfl
#PBS -V
cd $PBS_O_WORKDIR

csh ./AVERAGING_SCRIPTS/VPREM_022_average.csh
