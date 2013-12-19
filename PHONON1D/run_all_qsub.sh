#!/bin/tcsh


qsub CEES_QSUB_SCRIPTS/qsub_BM_EARTH_0100_SPIKE_10_B.sh
sleep 1
qsub CEES_QSUB_SCRIPTS/qsub_BM_EARTH_0100_SPIKE_07_B.sh
sleep 1
