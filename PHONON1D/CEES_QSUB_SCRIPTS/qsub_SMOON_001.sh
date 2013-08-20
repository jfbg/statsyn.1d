#!/bin/tcsh
#PBS -N SMOON_001
#PBS -l nodes=1:ppn=16
#PBS -q jfl
#PBS -V
cd $PBS_O_WORKDIR

csh SCRIPTS_CEES/SMOON_001.csh
