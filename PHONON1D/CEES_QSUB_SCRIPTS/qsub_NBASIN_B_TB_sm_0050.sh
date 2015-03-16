#!/bin/tcsh
#PBS -N NBASIN_B_TB_sm_0050
#PBS -l nodes=1:ppn=16
#PBS -q jfl
#PBS -V
cd $PBS_O_WORKDIR

csh SCRIPTS_BASIN/NBASIN_B_TB_sm_0050.csh
