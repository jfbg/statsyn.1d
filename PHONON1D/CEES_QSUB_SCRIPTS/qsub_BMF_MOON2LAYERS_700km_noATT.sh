#!/bin/tcsh
#PBS -N BMF_MOON2LAYERS_700km_noATT
#PBS -l nodes=1:ppn=16
#PBS -q jfl
#PBS -V
cd $PBS_O_WORKDIR

csh SCRIPTS_CEES/BMF_MOON2LAYERS_700km_noATT.csh
