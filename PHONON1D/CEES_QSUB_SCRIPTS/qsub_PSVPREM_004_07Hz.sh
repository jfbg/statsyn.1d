#!/bin/tcsh
#PBS -N PSVPREM_004_07Hz
#PBS -l nodes=1:ppn=16
#PBS -q jfl
#PBS -V
cd $PBS_O_WORKDIR

csh SCRIPTS_CEES/PSVPREM_004_07Hz.csh
