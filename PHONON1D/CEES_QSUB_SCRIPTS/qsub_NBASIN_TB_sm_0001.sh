#!/bin/tcsh
#PBS -N NBASIN_TB_sm_0001
#PBS -l nodes=1:ppn=16
#PBS -q jfl
#PBS -V
cd $PBS_O_WORKDIR

csh SCRIPTS_BASIN/NBASIN_TB_sm_0001.csh
