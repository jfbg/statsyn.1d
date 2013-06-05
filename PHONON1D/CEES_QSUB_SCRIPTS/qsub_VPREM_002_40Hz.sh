#!/bin/tcsh
#PBS -N VPREM_002_40Hz
#PBS -l nodes=1:ppn=16
#PBS -q jfl
#PBS -V
cd $PBS_O_WORKDIR

csh SCRIPTS_CEES/VPREM_002_40Hz.csh
