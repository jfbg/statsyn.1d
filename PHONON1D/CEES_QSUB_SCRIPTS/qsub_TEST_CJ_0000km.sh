#!/bin/tcsh
#PBS -N TEST_CJ_0000km
#PBS -l nodes=1:ppn=16
#PBS -q jfl
#PBS -V
cd $PBS_O_WORKDIR

csh SCRIPTS_CEES/TEST_CJ_0000km.csh
