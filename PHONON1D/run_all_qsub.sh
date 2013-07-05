#!/bin/tcsh


qsub CEES_QSUB_SCRIPTS/qsub_VPREM_nCORE_001_07Hz.sh
sleep 4
qsub CEES_QSUB_SCRIPTS/qsub_VPREM_nCORE_001_40Hz.sh
sleep 4
qsub CEES_QSUB_SCRIPTS/qsub_VPREM_024_07Hz.sh
sleep 4
qsub CEES_QSUB_SCRIPTS/qsub_VPREM_024_40Hz.sh
sleep 4
