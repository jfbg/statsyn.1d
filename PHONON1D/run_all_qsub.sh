#!/bin/tcsh


qsub CEES_QSUB_SCRIPTS/qsub_BM_EARTH_0100_SPIKE_noATT.sh
sleep 1
qsub CEES_QSUB_SCRIPTS/qsub_BM_EARTH_0100_SPIKE.sh
sleep 1
