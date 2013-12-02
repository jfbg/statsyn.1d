#!/bin/tcsh
#PBS -N RVPREM_001_07Hz
#PBS -l nodes=1:ppn=16
#PBS -q jfl
#PBS -V
cd $PBS_O_WORKDIR

csh SCRIPTS_CEES/RVPREM_001_07Hz.csh
