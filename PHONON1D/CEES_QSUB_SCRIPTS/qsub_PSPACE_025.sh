#!/bin/tcsh
#PBS -N PSPACE_025
#PBS -l nodes=1:ppn=16
#PBS -q jfl
#PBS -V
cd $PBS_O_WORKDIR

csh SCRIPTS_CEES/PSPACE_025.csh
