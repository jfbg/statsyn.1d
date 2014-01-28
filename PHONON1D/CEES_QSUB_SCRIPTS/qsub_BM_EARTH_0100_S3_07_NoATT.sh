#!/bin/tcsh
#PBS -N BM_EARTH_0100_S3_07_NoATT
#PBS -l nodes=1:ppn=16
#PBS -q jfl
#PBS -V
cd $PBS_O_WORKDIR

csh SCRIPTS_CEES/BM_EARTH_0100_S3_07_NoATT.csh
