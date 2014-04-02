#!/bin/tcsh
#PBS -N PBASIN_102_0000km_5Hz
#PBS -l nodes=1:ppn=16
#PBS -q jfl
#PBS -V
cd $PBS_O_WORKDIR

csh SCRIPTS_BASIN/PBASIN_102_0000km_5Hz.csh
