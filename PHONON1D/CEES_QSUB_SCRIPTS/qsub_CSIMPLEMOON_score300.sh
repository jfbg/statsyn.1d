#!/bin/tcsh
#PBS -N CSIMPLEMOON_score300
#PBS -l nodes=1:ppn=16
#PBS -q jfl
#PBS -V
cd $PBS_O_WORKDIR

csh SCRIPTS_CEES/CSIMPLEMOON_score300.csh
