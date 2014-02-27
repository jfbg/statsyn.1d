#!/bin/tcsh
#PBS -N BMC_MOON2LAYERS_700km_noATT_AllPs
#PBS -l nodes=1:ppn=16
#PBS -q jfl
#PBS -V
cd $PBS_O_WORKDIR

csh SCRIPTS_CEES/BMC_MOON2LAYERS_700km_noATT_AllPs.csh
