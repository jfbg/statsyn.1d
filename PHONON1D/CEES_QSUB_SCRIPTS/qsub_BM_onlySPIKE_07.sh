#!/bin/tcsh
#PBS -N BM_onlySPIKE_07
#PBS -l nodes=1:ppn=16
#PBS -q jfl
#PBS -V
cd $PBS_O_WORKDIR

csh SCRIPTS_CEES/BM_onlySPIKE_07.csh
