#!/bin/tcsh
#PBS -N CVPREM_003_07Hz
#PBS -l nodes=1:ppn=16
#PBS -q jfl
#PBS -V
cd $PBS_O_WORKDIR

csh SCRIPTS_CEES/CVPREM_003_07Hz.csh
