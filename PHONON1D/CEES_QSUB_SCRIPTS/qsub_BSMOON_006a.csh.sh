#!/bin/tcsh
#PBS -N BSMOON_006a.csh
#PBS -l nodes=1:ppn=16
#PBS -q jfl
#PBS -V
cd $PBS_O_WORKDIR

csh SCRIPTS_CEES/BSMOON_006a.csh.csh
