#!/bin/tcsh
#PBS -N generate80spikes
#PBS -l nodes=1:ppn=16
#PBS -q jfl
#PBS -V
cd $PBS_O_WORKDIR

csh SCRIPTS_CEES/generate80spikes.csh
