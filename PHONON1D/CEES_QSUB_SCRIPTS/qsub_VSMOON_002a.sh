#!/bin/tcsh
#PBS -N VSMOON_002a
#PBS -l nodes=1:ppn=16
#PBS -q jfl
#PBS -V
cd $PBS_O_WORKDIR

csh SCRIPTS_CEES/VSMOON_002a.csh
