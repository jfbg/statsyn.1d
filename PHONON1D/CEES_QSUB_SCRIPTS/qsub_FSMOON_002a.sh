#!/bin/tcsh
#PBS -N FSMOON_002a
#PBS -l nodes=1:ppn=16
#PBS -q jfl
#PBS -V
cd $PBS_O_WORKDIR

csh SCRIPTS_CEES/FSMOON_002a.csh
