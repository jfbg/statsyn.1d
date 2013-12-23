#!/bin/tcsh
#PBS -N PBASIN_002_5Hz
#PBS -l nodes=1:ppn=16
#PBS -q jfl
#PBS -V
cd $PBS_O_WORKDIR

csh SCRIPTS_BASIN/PBASIN_002_5Hz.csh
