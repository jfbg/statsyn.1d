#!/bin/tcsh
#PBS -N CSMOON_001
#PBS -l nodes=1:ppn=16
#PBS -q jfl
#PBS -V
cd $PBS_O_WORKDIR

csh SCRIPTS_CEES/CSMOON_001.csh
