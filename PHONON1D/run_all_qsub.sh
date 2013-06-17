#!/bin/tcsh


qsub CEES_QSUB_SCRIPTS/qsub_VPREM_nVL_013_07Hz.sh
sleep 4
qsub CEES_QSUB_SCRIPTS/qsub_VPREM_nVL_013_40Hz.sh
sleep 4
