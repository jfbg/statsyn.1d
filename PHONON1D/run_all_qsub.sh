#!/bin/tcsh


qsub CEES_QSUB_SCRIPTS/qsub_BM_EARTH_0100_SINE.sh
sleep 1
qsub CEES_QSUB_SCRIPTS/qsub_BM_EARTH_0100_SINE_noATT.sh
sleep 1
qsub CEES_QSUB_SCRIPTS/qsub_BM_EARTH_0100_SINE_B.sh
sleep 1
