#!/bin/tcsh
#PBS -N CSIMPLEMOON_basic_FirstS
#PBS -l nodes=1:ppn=16
#PBS -q jfl
#PBS -V
cd $PBS_O_WORKDIR

csh SCRIPTS_CEES/CSIMPLEMOON_basic_FirstS.csh
