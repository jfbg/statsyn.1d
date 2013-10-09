#!/bin/tcsh


qsub CEES_QSUB_SCRIPTS/qsub_CVPREM_001b_40Hz.sh
sleep 4
qsub CEES_QSUB_SCRIPTS/qsub_CVPREM_001b_07Hz.sh
sleep 4
