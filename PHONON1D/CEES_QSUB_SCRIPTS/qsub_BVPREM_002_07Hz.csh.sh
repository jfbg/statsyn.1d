#!/bin/tcsh
#PBS -N BVPREM_002_07Hz.csh
#PBS -l nodes=1:ppn=16
#PBS -q jfl
#PBS -V
cd $PBS_O_WORKDIR

csh SCRIPTS_CEES/BVPREM_002_07Hz.csh.csh
