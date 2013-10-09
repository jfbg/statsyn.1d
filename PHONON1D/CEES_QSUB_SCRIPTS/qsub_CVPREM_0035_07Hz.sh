#!/bin/tcsh
#PBS -N CVPREM_0035_07Hz
#PBS -l nodes=1:ppn=16
#PBS -q jfl
#PBS -V
cd $PBS_O_WORKDIR

csh SCRIPTS_CEES/CVPREM_0035_07Hz.csh
