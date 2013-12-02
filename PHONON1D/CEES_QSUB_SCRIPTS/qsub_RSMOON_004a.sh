#!/bin/tcsh
#PBS -N RSMOON_004a
#PBS -l nodes=1:ppn=16
#PBS -q jfl
#PBS -V
cd $PBS_O_WORKDIR

csh SCRIPTS_CEES/RSMOON_004a.csh
