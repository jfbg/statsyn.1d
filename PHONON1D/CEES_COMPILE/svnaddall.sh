#!/bin/tcsh

cd /Users/jguertin/Documents/Research/Moon/Phonon1D/PHONON1D/CEES_COMPILE/AVERAGING_SCRIPTS 
svn add -q *.csh
cd /Users/jguertin/Documents/Research/Moon/Phonon1D/PHONON1D/CEES_COMPILE/LISTS 
svn add -q *.list
cd /Users/jguertin/Documents/Research/Moon/Phonon1D/PHONON1D/CEES_COMPILE/QSUB_SCRIPTS 
svn add -q *.sh
cd /Users/jguertin/Documents/Research/Moon/Phonon1D/PHONON1D/CEES_COMPILE 
svn add -q *.list

svn ci -m ''
