#!/bin/tcsh
#PBS -N PVPREM_001_07Hz_1000km_noscat
#PBS -l nodes=1:ppn=16
#PBS -q jfl
#PBS -V
cd $PBS_O_WORKDIR

csh SCRIPTS_CEES/PVPREM_001_07Hz_1000km_noscat.csh
