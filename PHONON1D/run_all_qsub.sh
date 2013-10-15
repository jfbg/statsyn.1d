#!/bin/tcsh


qsub CEES_QSUB_SCRIPTS/qsub_CVPREM_003_07Hz.sh
sleep 4
qsub CEES_QSUB_SCRIPTS/qsub_CVPREM_002_07Hz.sh
sleep 4
qsub CEES_QSUB_SCRIPTS/qsub_CVPREM_001_07Hz.sh
sleep 4
qsub CEES_QSUB_SCRIPTS/qsub_CVPREM_001b_07Hz.sh
sleep 4
