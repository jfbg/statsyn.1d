#!/bin/tcsh
#PBS -N PSVPREM_064_060_40Hz_1000km
#PBS -l nodes=1:ppn=16
#PBS -q jfl
#PBS -V
cd $PBS_O_WORKDIR

csh SCRIPTS_CEES/PSVPREM_064_060_40Hz_1000km.csh
