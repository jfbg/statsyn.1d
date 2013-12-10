#!/bin/tcsh
qsub ./QSUB_SCRIPTS/qsub_PSMOON_001_noscat_average.sh
qsub ./QSUB_SCRIPTS/qsub_PSMOON_012a_noscat_average.sh
qsub ./QSUB_SCRIPTS/qsub_PVPREM_001_noscat_average.sh
qsub ./QSUB_SCRIPTS/qsub_BM_EARTH_0100_SPIKE_noATT_average.sh
qsub ./QSUB_SCRIPTS/qsub_BM_EARTH_0100_SPIKE_average.sh
