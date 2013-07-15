#!/bin/tcsh


qsub CEES_QSUB_SCRIPTS/qsub_VPREM_062_07Hz.sh
sleep 4
qsub CEES_QSUB_SCRIPTS/qsub_VPREM_062_40Hz.sh
sleep 4
qsub CEES_QSUB_SCRIPTS/qsub_VPREM_S3_001C_40Hz.sh
sleep 4
qsub CEES_QSUB_SCRIPTS/qsub_VPREM_S3_061_40Hz.sh
sleep 4
qsub CEES_QSUB_SCRIPTS/qsub_VPREM_S3_062_40Hz.sh
sleep 4
qsub CEES_QSUB_SCRIPTS/qsub_VPREM_nCORE_012_07Hz.sh
sleep 4
qsub CEES_QSUB_SCRIPTS/qsub_VPREM_nCORE_012_40Hz.sh
sleep 4
