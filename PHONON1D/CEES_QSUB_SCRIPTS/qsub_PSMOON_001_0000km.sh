#!/bin/tcsh
#PBS -N PSMOON_001_0000km
#PBS -l nodes=1:ppn=16
#PBS -q jfl
#PBS -V
cd $PBS_O_WORKDIR

csh SCRIPTS_CEES/PSMOON_001_0000km.csh
