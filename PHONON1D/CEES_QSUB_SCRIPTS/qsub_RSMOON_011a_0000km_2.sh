#!/bin/tcsh
#PBS -N RSMOON_011a_0000km_2
#PBS -l nodes=1:ppn=16
#PBS -q jfl
#PBS -V
cd $PBS_O_WORKDIR

csh SCRIPTS_CEES/RSMOON_011a_0000km_2.csh
