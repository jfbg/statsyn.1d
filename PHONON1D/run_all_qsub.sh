#!/bin/tcsh


qsub CEES_QSUB_SCRIPTS/qsub_VPREM_071_07Hz.sh
sleep 4
qsub CEES_QSUB_SCRIPTS/qsub_VPREM_071_40Hz.sh
sleep 4
qsub CEES_QSUB_SCRIPTS/qsub_VPREM_072_07Hz.sh
sleep 4
qsub CEES_QSUB_SCRIPTS/qsub_VPREM_072_40Hz.sh
sleep 4
qsub CEES_QSUB_SCRIPTS/qsub_VPREM_073_07Hz.sh
sleep 4
qsub CEES_QSUB_SCRIPTS/qsub_VPREM_073_40Hz.sh
sleep 4
