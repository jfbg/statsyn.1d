#!/bin/tcsh
#PBS -N PSVPREM_031_40Hz
#PBS -l nodes=1:ppn=16
#PBS -q jfl
#PBS -V
cd $PBS_O_WORKDIR

csh SCRIPTS_CEES/PSVPREM_031_40Hz.csh
