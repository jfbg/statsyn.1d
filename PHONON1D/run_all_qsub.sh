#!/bin/tcsh


qsub CEES_QSUB_SCRIPTS/qsub_BVPREM_001_07Hz.sh
sleep 4
qsub CEES_QSUB_SCRIPTS/qsub_BVPREM_001_40Hz.sh
sleep 4
qsub CEES_QSUB_SCRIPTS/qsub_BVPREM_002_07Hz.sh
sleep 4
qsub CEES_QSUB_SCRIPTS/qsub_BVPREM_002_40Hz.sh
sleep 4
