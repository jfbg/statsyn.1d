#!/bin/tcsh
#PBS -N NBASIN_TB_vp_0100
#PBS -l nodes=1:ppn=16
#PBS -q jfl
#PBS -V
cd $PBS_O_WORKDIR

csh SCRIPTS_BASIN/NBASIN_TB_vp_0100.csh
