#!/bin/tcsh
#PBS -N PSVPREM_042_07Hz
#PBS -l nodes=1:ppn=16
#PBS -q jfl
#PBS -V
cd $PBS_O_WORKDIR

csh SCRIPTS_CEES/PSVPREM_042_07Hz.csh
