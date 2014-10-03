#!/bin/tcsh
#PBS -N PBASIN_CS_2330B_1000km_5Hz
#PBS -l nodes=1:ppn=16
#PBS -q jfl
#PBS -V
cd $PBS_O_WORKDIR

csh SCRIPTS_BASIN/PBASIN_CS_2330B_1000km_5Hz.csh
