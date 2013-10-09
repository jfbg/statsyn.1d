#!/bin/tcsh


qsub CEES_QSUB_SCRIPTS/qsub_CVPREM_003_40Hz.sh
sleep 4
qsub CEES_QSUB_SCRIPTS/qsub_CVPREM_0035_07Hz.sh
sleep 4
