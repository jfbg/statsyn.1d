#!/bin/tcsh
#PBS -N TRACK_VSMOON_001
#PBS -l nodes=1:ppn=16
#PBS -q jfl
#PBS -V
cd $PBS_O_WORKDIR

csh SCRIPTS_CEES/TRACK_VSMOON_001.csh
