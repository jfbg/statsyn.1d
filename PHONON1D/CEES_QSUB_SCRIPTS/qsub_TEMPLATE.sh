#!/bin/tcsh
#PBS -N PHONON1D_TEST130603
#PBS -l nodes=1:ppn=16 
#PBS -q jfl
#PBS -V
cd $PBS_O_WORKDIR

csh SCRIPTS_CEES/TEST130603.csh
