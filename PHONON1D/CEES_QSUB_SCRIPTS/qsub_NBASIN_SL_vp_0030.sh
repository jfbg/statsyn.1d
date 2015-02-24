#!/bin/tcsh
#PBS -N NBASIN_SL_vp_0030
#PBS -l nodes=1:ppn=16
#PBS -q jfl
#PBS -V
cd $PBS_O_WORKDIR

csh SCRIPTS_BASIN/NBASIN_SL_vp_0030.csh
