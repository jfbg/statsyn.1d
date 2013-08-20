#!/bin/tcsh
#PBS -N SMOON_008a
#PBS -l nodes=1:ppn=16
#PBS -q jfl
#PBS -V
cd $PBS_O_WORKDIR

csh SCRIPTS_CEES/SMOON_008a.csh
