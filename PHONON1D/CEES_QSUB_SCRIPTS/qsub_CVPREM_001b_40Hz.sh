#!/bin/tcsh
#PBS -N CVPREM_001b_40Hz
#PBS -l nodes=1:ppn=16
#PBS -q jfl
#PBS -V
cd $PBS_O_WORKDIR

csh SCRIPTS_CEES/CVPREM_001b_40Hz.csh
