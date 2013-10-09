#!/bin/tcsh


qsub CEES_QSUB_SCRIPTS/qsub_CVPREM_002_40Hz.sh
sleep 4
qsub CEES_QSUB_SCRIPTS/qsub_CVPREM_002_07Hz.sh
sleep 4
