#!/bin/tcsh
#PBS -N COMPILE_VPREM_nVL_004
#PBS -l nodes=1:ppn=1
#PBS -q jfl
#PBS -V
cd $PBS_O_WORKDIR

csh ./AVERAGING_SCRIPTS/VPREM_nVL_004_average.csh
