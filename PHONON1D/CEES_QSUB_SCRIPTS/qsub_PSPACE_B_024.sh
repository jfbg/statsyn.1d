#!/bin/tcsh
#PBS -N PSPACE_B_024
#PBS -l nodes=1:ppn=16
#PBS -q jfl
#PBS -V
cd $PBS_O_WORKDIR

csh SCRIPTS_CEES/PSPACE_B_024.csh
