#!/bin/tcsh
#PBS -N CSIMPLEMOON_vlvl
#PBS -l nodes=1:ppn=16
#PBS -q jfl
#PBS -V
cd $PBS_O_WORKDIR

csh SCRIPTS_CEES/CSIMPLEMOON_vlvl.csh
