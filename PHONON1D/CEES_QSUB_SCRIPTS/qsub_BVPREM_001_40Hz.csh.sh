#!/bin/tcsh
#PBS -N BVPREM_001_40Hz.csh
#PBS -l nodes=1:ppn=16
#PBS -q jfl
#PBS -V
cd $PBS_O_WORKDIR

csh SCRIPTS_CEES/BVPREM_001_40Hz.csh.csh
