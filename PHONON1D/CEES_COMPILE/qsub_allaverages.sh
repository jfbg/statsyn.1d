#!/bin/tcsh
qsub ./QSUB_SCRIPTS/qsub_BM_EARTH_0100_SPIKE_average.sh
qsub ./QSUB_SCRIPTS/qsub_BM_EARTH_0100_SPIKE_noATT_average.sh
qsub ./QSUB_SCRIPTS/qsub_BM_EARTH_0030_SPIKE_average.sh
qsub ./QSUB_SCRIPTS/qsub_BM_EARTHPREM_SPIKE_100km_EXP_noATT_RS_average.sh
